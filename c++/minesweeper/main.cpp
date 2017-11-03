/*
  Simple minesweeper game implementation in pure C++.

  Using GNU style guide, found at
  https://www.gnu.org/prep/standards/standards.html

  Miloslav Ciz, 2017
  WTFPL license
 */

#include <iostream>
#include <string>
#include <vector>

using namespace std;

#define GAMEFIELD_DEFAULT_WIDTH 30
#define GAMEFIELD_DEFAULT_HEIGHT 10

#define DEFAULT_MINES 20
#define DEFAULT_SEED 0

#define KEY_UP 'w'
#define KEY_RIGHT 'd'
#define KEY_DOWN 's'
#define KEY_LEFT 'a'
#define KEY_REVEAL 'e'
#define KEY_MARK 'r'
#define KEY_QUIT 'q'

#define PARAM_WIDTH 'w'
#define PARAM_HEIGHT 'h'
#define PARAM_MINES 'm'
#define PARAM_SEED 's'

#define FRAME_PRECEDING_NEWLINES 80

#define min(x,y) ((x) < (y) ? x : y)
#define max(x,y) ((x) > (y) ? x : y)

typedef enum
  {
    STATE_HIDDEN,
    STATE_REVEALED,
    STATE_MARKED
  } GamefieldState;

typedef enum
  {
    RESULT_NONE,
    RESULT_LOST,
    RESULT_WON
  } GameResult;

class GameFieldSquare
  {
    public:
      GameFieldSquare()
        {
          this->has_mine = false;
          this->mine_neighbours = 0;
          this->state = STATE_HIDDEN;
        }

      bool has_mine;
      int mine_neighbours;
      GamefieldState state; 
  };

typedef vector<vector<Gamefield>> GameField;

class RandomGenerator
  {
    public:
      RandomGenerator()
        {
          this->last_random = 0;
        }

      // custom random generator to keep consistency between implementations

      unsigned int random_number()             
        {
          // taken from from glibc
          this->last_random = (this->last_random * 1103515245 + 12345) % 2147483647; 
          return this->last_random;
        }

      void set_seed(int seed)
        {
          this->last_random = seed;
        }

    protected:
      int last_random;
  }

class MinesweeperGame
  {
    public:
      MinesweeperGame(int argc, char **argv)
        {
          this->ok = true;
          int w, h, m, s;

          w = GAMEFIELD_DEFAULT_WIDTH;
          h = GAMEFIELD_DEFAULT_HEIGHT;
          m = DEFAULT_MINES;
          s = DEFAULT_SEED;

          // parse parameters

          for (int i = 1; i < argc; i++)
            {
              if (argv[i][0] != '-' || strlen(argv[i]) < 3)
                this->ok = false;
              else
                switch (argv[i][1])
                  {
                    case PARAM_WIDTH: error = sscanf(argv[i] + 2,"%d",&w) == 0; break;
                    case PARAM_HEIGHT: error = sscanf(argv[i] + 2,"%d",&h) == 0; break;
                    case PARAM_MINES: error = sscanf(argv[i] + 2,"%d",&m) == 0; break;
                    case PARAM_SEED: error = sscanf(argv[i] + 2,"%d",&s) == 0; break;
                    default: this->ok = false; break;
                  }
              
              if (!this->ok)
                {
                  cerr << "error at parameter " << i << ": " << argv[i] << "." << endl;
                  print_help();
                  return 0;
                }
            }

          this->random_generator.set_seed(s);

          this->cursor_x = w / 2;
          this->cursor_y = h / 2;

          for (int x = 0; x < w; x++)
            for (int y = 0; y < h; y++)
              this->gamefield[x].push_back(GameFieldSquare);            

          this->result = RESULT_NONE;

          // place mines:

          for (int i = 0; i < m; i++)
            {
              while (true)
                {
                  int x = this->random_generator.random_number() % w;
                  int y = this->random_generator.random_number() % h;

                  if (!this->gamefield[x][y].has_mine)
                    {
                      this->gamefield[x][y].has_mine = 1;
                      break;
                    }
                }
            }

          // compute field numbers

          for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++)
              if (this->gamefield[x][y].has_mine)
                this->for_each_neighbour(x,y,
                    [](Gamefield *gf, float x, float y)   // lambda
                      {
    // TODO                               
                      } 
                  );

          return 1;
        }

      bool ok()
        {
          return this->ok;
        }

      void run()
        {
          t_gamestate gamestate;

          if (!init_game(argc,argv,&gamestate))
            return 1;

          while (1)   // main loop
            {
              draw_frame(&gamestate);

              if (gamestate.result == RESULT_NONE)
                printf("provide input: ");
              else
                {
                  if (gamestate.result == RESULT_LOST)
                    printf("game over");
                  else
                    printf("victory");

                  getchar();
                  break;
                }

              char c = getchar();

              if (c == KEY_QUIT)
                break;

              game_step(&gamestate,c);
            }
        }

    protected:
      bool ok;
      int cursor_x;
      int cursor_y;
      int mines;
      int marked;
      GameResult result;
      Gamefield gamefield;
      RandomGenerator random_generator;
  };

void for_each_neighbour(t_gamestate *gamestate, int x, int y, void (func)(t_gamestate *, int x, int y))
  {
    int offsets_x[] = {1,1,0,-1,-1,-1,0,1};
    int offsets_y[] = {0,-1,-1,-1,0,1,1,1};

    for (int i = 0; i < 8; i++)   // 8-neighbourhood
      {
        int x2 = x + offsets_x[i];
        int y2 = y + offsets_y[i];

        if (x2 < 0 || y2 < 0 ||  // outside gamefield?
            x2 >= gamestate->gamefield_width || y2 >= gamestate->gamefield_height)
          continue;

        func(gamestate,x2,y2);
      }
  }

void increment_mines(t_gamestate *gamestate, int x, int y)
  {
    gamestate->gamefield[x][y].mine_neighbours++;
  }

void reveal_field(t_gamestate *gamestate, int x, int y)
  {
    t_gamefield *f = &(gamestate->gamefield[x][y]);

    if (f->state == STATE_REVEALED)
      return;

    f->state = STATE_REVEALED;

    if (f->mine_neighbours == 0) // recursively reveal neighbours
      for_each_neighbour(gamestate,x,y,reveal_field);
  }

void print_help()
  {
    cout << "Simple minesweeper game implemented in C++." << endl;
    cout << "In-game commands:" << endl;
    cout << "  -" << KEY_UP << ": move up" << endl;
    cout << "  -" << KEY_RIGHT << ": move right" << endl;
    cout << "  -" << KEY_DOWN << ": move down" << endl;
    cout << "  -" << KEY_LEFT << ": move left" << endl;
    cout << "  -" << KEY_RIGHT << ": reveal" << endl;
    cout << "  -" << KEY_MARK << ": mark" << endl;
    cout << "  -" << KEY_QUIT << ": quit" << endl;
    cout << "CLI parameters:" << endl;
    cout << "  -" << PARAM_WIDTH << "N: set gamefield width to N" << endl;
    cout << "  -" << PARAM_HEIGHT << "N: set gamefield height to N" << endl;
    cout << "  -" << PARAM_MINES << "N: set number of mines to N" << endl;
    cout << "  -" << PARAM_SEED << "N: set random seed N" << endl;
  }

int init_game(int argc, char **argv, t_gamestate *gamestate)
  {
  }

void draw_frame(t_gamestate *gamestate)
  {
    for (int i = 0; i < FRAME_PRECEDING_NEWLINES; i++)
      printf("\n");

    for (int y = -1; y <= gamestate->gamefield_height; y++)
      {
        int horizontal_line = y == -1 || y == gamestate->gamefield_height;

        char vertical_separator = horizontal_line ? 'o' : '|';

        printf(" %c",vertical_separator);

        for (int x = 0; x < gamestate->gamefield_width; x++)
          {
            char c = '.';

            if (horizontal_line)
              c = '-';
            else if (x == gamestate->cursor_x && y == gamestate->cursor_y)
              c = '>';
            else
              {
                t_gamefield f = gamestate->gamefield[x][y];

                if (f.state == STATE_REVEALED)
                  c = f.mine_neighbours > 0 ? '0' + f.mine_neighbours : ' ';
                else if (f.state == STATE_MARKED)
                  c = '#';
              }

           if (gamestate->result == RESULT_LOST && gamestate->gamefield[x][y].has_mine)
             c = '*';

            printf("%c",c);
          }

        printf("%c\n",vertical_separator);
      }

    printf("\nmarked: %d / %d\n\n",gamestate->marked,gamestate->mines);
  }

void game_step(t_gamestate *gamestate, char input_key)
  {
    t_gamefield *f = &(gamestate->gamefield[gamestate->cursor_x][gamestate->cursor_y]);

    switch (input_key)
      {
        case KEY_UP:
          gamestate->cursor_y = max(0,gamestate->cursor_y - 1);
          break; 

        case KEY_RIGHT:
          gamestate->cursor_x = min(gamestate->gamefield_width - 1,gamestate->cursor_x + 1);
          break; 

        case KEY_DOWN:
          gamestate->cursor_y = min(gamestate->gamefield_height - 1,gamestate->cursor_y + 1);
          break;
 
        case KEY_LEFT:
          gamestate->cursor_x = max(0,gamestate->cursor_x - 1);
          break;
 
        case KEY_REVEAL:
          if (f->state != STATE_MARKED)
            {
              if (f->has_mine)
                gamestate->result = RESULT_LOST;
              else
                {
                  reveal_field(gamestate,gamestate->cursor_x,gamestate->cursor_y);

                  // check victory

                  gamestate->result = RESULT_WON;

                  for (int y = 0; y < gamestate->gamefield_height; y++)
                    {
                      for (int x = 0; x < gamestate->gamefield_width; x++)
                        {
                          t_gamefield f2 = gamestate->gamefield[x][y];

                          if (!f2.has_mine && f2.state != STATE_REVEALED)
                            {
                              gamestate->result = RESULT_NONE;
                              break;
                            }
                        }

                      if (gamestate->result == RESULT_NONE)
                        break;
                    }
                }
            }

          break; 

        case KEY_MARK:
          if (f->state != STATE_REVEALED)
            {
              if (f->state == STATE_HIDDEN)
                {
                  f->state = STATE_MARKED;
                  gamestate->marked++;
                }
              else
                {
                  f->state = STATE_HIDDEN;
                  gamestate->marked--;
                }
            }

          break; 

        default:
          break;
      }
  }

int main(int argc, char **argv)
  {
    MinesweeperGame game(argc,argv);

    if (!game.ok())
      return 1;

    game.run();
    return 0;
  }
