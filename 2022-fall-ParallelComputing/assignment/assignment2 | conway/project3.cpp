#include<stdio.h>
#include<stdlib.h>
#include <mpi.h>
#include <fstream>
#include <string>
#include <sstream>

#define ALIVE 1
#define DEAD 0

using namespace std;

/* read_file read input file and set basic arguments */
void read_file(char* argv[], int &board_size, int &gens, int &ghosts, string & s_board){
    ifstream readFile;
    readFile.open(argv[1]);
    if(readFile.is_open()){
        string line;

        // read board size
        getline(readFile, line);
        board_size = atoi(line.c_str());

        // read the number of generation 
        getline(readFile, line);
        gens = atoi(line.c_str());

        // read ghost size
        getline(readFile, line);
        ghosts = atoi(line.c_str());

        // read initial states
        while(getline(readFile, line)){
            s_board += line + "\n";
        }
    }
    readFile.close(); 
}

/* print_board prints contents of dynamically allocated board */
void print_board(int rows, int cols, bool** board) {
    printf(">>>>> print board!\n");
    for(int i=0; i<rows; i++){
        printf("[");
        for(int j=0; j<cols; j++){
            printf("%d, ",board[i][j]);
        }
        printf("]\n");
    }
    printf("\n");
}

/* get_init_board parse string input of initial state and translate it into 1 or 0*/
void get_init_board(int n, bool** board, char* c_board){
    int i=0;
    char* row = strtok(c_board, "\n");
    while(row != NULL){
        for(int j=0; j<n; j++){
            char cell = row[j];
            if(cell == '.'){
                 board[i][j] = DEAD;
            }
            else board[i][j] = ALIVE;
        }
        row = strtok(NULL, "\n");
        i++;
    }    
}

/* update_state determines next state of given cell based on number of alive_neighbors and its current state (ALIVE | DEAD) */
void update_state(bool& next_state, bool cur_state, int alive_neighbors){
    if(cur_state == ALIVE && (alive_neighbors == 2 || alive_neighbors == 3)) next_state = ALIVE;
    else if(cur_state == DEAD && alive_neighbors == 3) next_state = ALIVE;
    else next_state = DEAD;
}

int main(int argc, char* argv[]) {
    
    
    
    
    
    /* Start MPI process */
    MPI_Init(&argc, &argv);

    int comm_size, my_rank;
    MPI_Status status;
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    /* Create 1D cartesian topologies */
    MPI_Comm RING_COMM;
    int dim[1] = {comm_size};
    int period[1] = {0};
    MPI_Cart_create(MPI_COMM_WORLD, 1, dim, period, 0, &RING_COMM);

    
    /* Set initial environment */
    int board_size, gens, ghosts;
    string s_board;
    read_file(argv, board_size, gens, ghosts, s_board);


    int my_row = board_size / comm_size; //my_row is # rows of decomposed board for specific node
    int my_col = board_size; //my_row is # cols of sliced board for specific node (all node has same my_col size)

    int rem = board_size % comm_size;
    if(my_rank < rem) my_row = board_size / comm_size + 1; //


    /* Allocate initial board and Initialize it */
    bool** board = new bool*[board_size]();
    for(int i=0; i<board_size; i++){
        board[i] = new bool[board_size]();
    }
    char * c_board = &*s_board.begin();

    get_init_board(board_size, board, c_board);


    /* Decompose initial board into my_board */
    bool my_board[my_row][my_col];
    int i = 0; // i indicates start index of each slice in board
    if(my_rank < rem) i = my_rank * my_row;
    else i = (rem)*(my_row+1) + (my_rank-rem) * my_row;

    for(int r=0; r<my_row; r++){
        for(int c=0; c<my_col; c++) my_board[r][c] = board[i+r][c];
    }

    /* Set boundary buffer
        >>> ghost section for duplicated boundary
        >>> comm section for communication-needed boundary
    */
    // both boundary array holds adjacent nodes boundary cells
    bool upper_boundary[my_col] = {0};
    bool lower_boundary[my_col] = {0};

    // Determine ghost section size
    int ghost_size = ghosts * ghosts;
    
    // maximum ghost size is 2 * my_col
    if(ghost_size> my_col*2) ghost_size = 2 * my_col;
    // maximum size of both upper and lower ghost section is my_col
    int n_upper_ghost, n_lower_ghost;
    if(my_rank == 0){
        n_upper_ghost = 0;
        n_lower_ghost = ghost_size;

        if(n_lower_ghost > my_col) n_lower_ghost = my_col;
    }else if(my_rank == comm_size -1){
        n_upper_ghost = ghost_size;
        n_lower_ghost = 0;

        if(n_upper_ghost>my_col) n_upper_ghost = my_col;
    }else{
        n_upper_ghost = ghost_size / 2;
        n_lower_ghost = ghost_size / 2;

        if(ghost_size%2==1) n_upper_ghost++;
    }

    // fill the ghost section
    if(n_upper_ghost!=0) for(int ug=0; ug<n_upper_ghost; ug++) upper_boundary[ug] = board[i-1][ug];
    if(n_lower_ghost!=0) for(int lg=0; lg<n_lower_ghost; lg++) lower_boundary[lg] = board[i+my_row][lg];

    /* Deallocate initial board */
    for(int i=0; i<board_size; i++){
        delete[] board[i];
    }
    delete[] board;


    // Determine comm section size
    int n_upper_comm, n_lower_comm;
    if(my_rank == 0){
        n_upper_comm = 0;
        n_lower_comm = my_col - n_lower_ghost;
    }else if(my_rank == comm_size -1){
        n_upper_comm = my_col - n_upper_ghost ;
        n_lower_comm = 0;
    }else{
        n_upper_comm = my_col - n_upper_ghost;
        n_lower_comm = my_col - n_lower_ghost;
    }


    /* Set communication metadata
        >>> comm_metadata holds send_count for adjacent nodes
        >>> comm_metadata[0] holds send_count for previous node, comm_metadata[1] holds send_count for next node
        >>> negative direction uses 110 tag, positive direction uses 130 tag 
    */
    int metadata_sc[2] = {0}; 
    if(my_rank == 0) MPI_Sendrecv(&n_lower_comm, 1, MPI_INT, my_rank + 1, 130, &metadata_sc[1], 1, MPI_INT, my_rank+1, 110, RING_COMM, &status);
    else if(my_rank == comm_size-1) MPI_Sendrecv(&n_upper_comm, 1, MPI_INT, my_rank-1, 110, &metadata_sc[0], 1, MPI_INT, my_rank-1, 130, RING_COMM, &status);
    else {
        int source, dest;
        MPI_Cart_shift(RING_COMM, 0, -1, &source, &dest);
        MPI_Sendrecv(&n_upper_comm, 1, MPI_INT, dest, 110, &metadata_sc[1], 1, MPI_INT, source, 110, RING_COMM, &status);
        MPI_Sendrecv(&n_lower_comm, 1, MPI_INT, source, 130, &metadata_sc[0], 1, MPI_INT, dest, 130, RING_COMM, &status);
    }
    
    
    

    /* Do game */
    for(int g=0; g<gens; g++){
        /* Communicate comm section in boundary array 
            >>> This process fills comm section which is not ghost cell
            >>> negative direction uses 230 tag, positive direction uses 290
        */
        if(ghost_size != my_col*2){
            if(my_rank == 0) MPI_Sendrecv(&my_board[my_row-1][my_col-metadata_sc[1]], metadata_sc[1], MPI_BYTE, my_rank +1, 290, &lower_boundary[my_col-n_lower_comm], n_lower_comm, MPI_BYTE, my_rank +1, 230, RING_COMM, &status);
            else if(my_rank == comm_size -1) MPI_Sendrecv(&my_board[0][my_col-metadata_sc[0]], metadata_sc[0],MPI_BYTE, my_rank -1, 230, &upper_boundary[my_col-n_upper_comm], n_upper_comm, MPI_BYTE, my_rank -1, 290, RING_COMM, &status);
            else{
                int source, dest;
                MPI_Cart_shift(RING_COMM, 0, -1, &source, &dest);
                MPI_Sendrecv(&my_board[0][my_col-metadata_sc[0]], metadata_sc[0], MPI_BYTE, dest, 230, &lower_boundary[my_col-n_lower_comm], n_lower_comm, MPI_BYTE, source, 230, RING_COMM, &status);
                MPI_Sendrecv(&my_board[my_row-1][my_col-metadata_sc[1]], metadata_sc[1], MPI_BYTE, source, 290, &upper_boundary[my_col-n_upper_comm], n_upper_comm, MPI_BYTE, dest, 290, RING_COMM, &status);
            }
        }
        

        /* Calculate state of next generation for each cell */
        bool updated_my_board[my_row][my_col];
        for(int r=0; r<my_row; r++){
            int alive_neighbors;
            for(int c=0; c<my_col; c++){
                // upper left corner
                if(r==0 && c==0){
                    if(my_rank == 0) alive_neighbors = my_board[r][c+1] + my_board[r+1][c] + my_board[r+1][c+1];
                    else alive_neighbors = upper_boundary[c] + upper_boundary[c+1] + my_board[r][c+1] + my_board[r+1][c] + my_board[r+1][c+1];
                }
                // upper right corner
                else if(r==0 && c==my_col-1){
                    if(my_rank == 0) alive_neighbors = my_board[r][c-1] + my_board[r+1][c-1] + my_board[r+1][c];
                    else alive_neighbors = upper_boundary[c-1] + upper_boundary[c] + my_board[r][c-1] + my_board[r+1][c-1] + my_board[r+1][c];
                }
                // lower left corner
                else if(r==my_row-1 && c==0){
                    if(my_rank == comm_size - 1) alive_neighbors = my_board[r-1][c] + my_board[r-1][c+1] + my_board[r][c+1];
                    else alive_neighbors = my_board[r-1][c] + my_board[r-1][c+1] + my_board[r][c+1] + lower_boundary[c] + lower_boundary[c+1];
                }
                // lower right corner
                else if(r == my_row-1 && c== my_col-1){
                    if(my_rank == comm_size -1) alive_neighbors = my_board[r-1][c-1] + my_board[r-1][c] + my_board[r][c-1];
                    else alive_neighbors = my_board[r-1][c-1] + my_board[r-1][c] + my_board[r][c-1] + lower_boundary[c-1] + lower_boundary[c];
                }
                // not corner but first column
                else if(c == 0)
                    alive_neighbors = my_board[r-1][c] + my_board[r-1][c+1] + my_board[r][c+1] + my_board[r+1][c] + my_board[r+1][c+1];
                // not corner but last column
                else if(c == my_col-1)
                    alive_neighbors = my_board[r-1][c-1] + my_board[r-1][c] + my_board[r][c-1] + my_board[r+1][c-1] + my_board[r+1][c];
                // not corner but first row
                else if(r == 0){
                    if(my_rank == 0) alive_neighbors = my_board[r][c-1] + my_board[r][c+1] + my_board[r+1][c-1] + my_board[r+1][c] + my_board[r+1][c+1];
                    else alive_neighbors = upper_boundary[c-1] + upper_boundary[c] + upper_boundary[c+1] + my_board[r][c-1] + my_board[r][c+1] + my_board[r+1][c-1] + my_board[r+1][c] + my_board[r+1][c+1];
                }
                // not corner but last row
                else if(r == my_row -1){
                    if(my_rank == comm_size -1) alive_neighbors = my_board[r][c-1] + my_board[r-1][c-1] + my_board[r-1][c] + my_board[r-1][c+1] + my_board[r][c+1];
                    else alive_neighbors = my_board[r][c-1] + my_board[r-1][c-1] + my_board[r-1][c] + my_board[r-1][c+1] + my_board[r][c+1] + lower_boundary[c+1] + lower_boundary[c] + lower_boundary[c-1];
                }
                // all 8 neighbors are in my_board
                else
                    alive_neighbors = my_board[r-1][c-1] + my_board[r-1][c] + my_board[r-1][c+1] + my_board[r][c-1] + my_board[r][c+1] + my_board[r+1][c-1] + my_board[r+1][c] + my_board[r+1][c+1];

                update_state(updated_my_board[r][c], my_board[r][c], alive_neighbors);
            }
        }

         /* Update my_board to next generation state */
        for(int r=0; r<my_row; r++){
            for(int c=0; c<my_col; c++){
                my_board[r][c] = updated_my_board[r][c];
            }
        }

        /* Communicate ghosts section in boundary array
            >>> this process updates ghost section to next state 
            >>> negative direction uses 310 tag, positive direction uses 370
        */
        if(my_rank == 0) MPI_Sendrecv(&my_board[my_row-1][0], my_col-metadata_sc[1], MPI_BYTE, my_rank + 1, 370, &lower_boundary[0], n_lower_ghost, MPI_BYTE, my_rank + 1, 310, RING_COMM, &status);
        else if(my_rank == comm_size -1) MPI_Sendrecv(&my_board[0][0], my_col-metadata_sc[0], MPI_BYTE, my_rank -1, 310, &upper_boundary[0], n_upper_ghost, MPI_BYTE, my_rank -1, 370, RING_COMM, &status);
        else{
            int source, dest;
            MPI_Cart_shift(RING_COMM, 0, -1, &source, &dest);
            MPI_Sendrecv(&my_board[0][0], my_col - metadata_sc[0], MPI_BYTE, dest, 310, &lower_boundary[0], n_lower_ghost, MPI_BYTE, source, 310, RING_COMM, &status);
            MPI_Sendrecv(&my_board[my_row-1][0], my_col - metadata_sc[1], MPI_BYTE, source, 370, &upper_boundary[0], n_upper_ghost, MPI_BYTE, dest, 370, RING_COMM, &status);
        }
    }

    /* Write to output file 
        >>> each node writes to output file in increasing rank order
    */
    int can_go = 1;
    if(my_rank == 0){
        ofstream outfile;
        outfile.open("./my_output.txt");
        for(int r=0; r<my_row; r++){
            string line = "";
            for(int c=0; c<my_col; c++){
                if(my_board[r][c] == DEAD) line += ".";
                else line += "#";
            }
            if(outfile.is_open()){
                outfile << line + "\n";
            }
        }
        outfile.close();
        MPI_Send(&can_go, 1, MPI_INT, 1, 410, RING_COMM);
    }
    else if(my_rank == comm_size -1){
        MPI_Recv(&can_go, 1, MPI_INT, my_rank-1, 410, RING_COMM, &status);

        ofstream outfile;
        outfile.open("./my_output.txt", ios_base::app);
        for(int r=0; r<my_row; r++){
            string line = "";
            for(int c=0; c<my_col; c++){
                if(my_board[r][c] == DEAD) line += ".";
                else line += "#";
            }
            if(outfile.is_open()){
                outfile << line + "\n";
            }
        }
        outfile.close();
    }
    else{
        MPI_Recv(&can_go, 1, MPI_INT, my_rank-1, 410, RING_COMM, &status);
        
        ofstream outfile;
        outfile.open("./my_output.txt", ios_base::app);
        for(int r=0; r<my_row; r++){
            string line = "";
            for(int c=0; c<my_col; c++){
                if(my_board[r][c] == DEAD) line += ".";
                else line += "#";
            }
            if(outfile.is_open()){
                outfile << line + "\n";
            }
        }
        outfile.close();
        MPI_Send(&can_go, 1, MPI_INT, my_rank+1, 410, RING_COMM);
    }
    

    double end = MPI_Wtime();
    MPI_Finalize();
    return 0;
}
