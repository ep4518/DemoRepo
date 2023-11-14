#include <cs50.h>
#include <stdio.h>
#include <string.h>

// Max number of candidates
#define MAX 9

// preferences[i][j] is number of voters who prefer i over j
int preferences[MAX][MAX];

// locked[i][j] means i is locked in over j
bool locked[MAX][MAX];

// Each pair has a winner, loser
typedef struct
{
    int winner;
    int loser;
} pair;

// Array of candidates
string candidates[MAX];
pair pairs[MAX * (MAX - 1) / 2];

int pair_count;
int candidate_count;

// Function prototypes
bool vote(int rank, string name, int ranks[]);
void record_preferences(int ranks[]);
void add_pairs(void);
void copy_pair(pair *dest, const pair *src);
void sort_pairs(void);
void lock_pairs(void);
bool forms_cycle(int winner, int loser, bool visited[], int n);
void print_winner(void);

int main(int argc, string argv[])
{
    // Check for invalid usage
    if (argc < 2)
    {
        printf("Usage: tideman [candidate ...]\n");
        return 1;
    }

    // Populate array of candidates
    candidate_count = argc - 1;
    if (candidate_count > MAX)
    {
        printf("Maximum number of candidates is %i\n", MAX);
        return 2;
    }
    for (int i = 0; i < candidate_count; i++)
    {
        candidates[i] = argv[i + 1];
    }

    // Clear graph of locked in pairs
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = 0; j < candidate_count; j++)
        {
            locked[i][j] = false;
        }
    }

    pair_count = 0;
    int voter_count = get_int("Number of voters: ");

    // Query for votes
    for (int i = 0; i < voter_count; i++)
    {
        // ranks[i] is voter's ith preference
        int ranks[candidate_count];

        // Query for each rank
        for (int j = 0; j < candidate_count; j++)
        {
            string name = get_string("Rank %i: ", j + 1);

            if (!vote(j, name, ranks))
            {
                printf("Invalid vote.\n");
                return 3;
            }
        }

        record_preferences(ranks);

        printf("\n");
    }

    add_pairs();
    sort_pairs();
    lock_pairs();
    print_winner();
    return 0;
}

// Update ranks given a new vote
bool vote(int rank, string name, int ranks[])
{
    // TODO
    // Initialise flag
    bool candidate_found = false;
    // Check for invalid vote
    for (int i = 0; i < candidate_count; i++)
    {
        if (strcmp(candidates[i], name) == 0)
        {
            // printf("Candidate: %s\n", candidates[i]);
            ranks[rank] = i;
            candidate_found = true;
            break;
        }
    }
    if (!candidate_found)
    {
        return false;
    }
    return true;
}

// Update preferences given one voter's ranks
void record_preferences(int ranks[])
{
    // TODO
    // preferences[i][j] is number of voters who prefer i over j
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = 0; j < candidate_count; j++)
        {
            if (i < j)
            {
                preferences[ranks[i]][ranks[j]] += 1;
            }
        }
    }

    return;
}

// Record pairs of candidates where one is preferred over the other
void add_pairs(void)
{
    // TODO
    int k = 0;
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = i + 1; j < candidate_count; j++) // Only compare each pair once
        {
            if (preferences[i][j] > preferences[j][i])
            {
                pairs[k].winner = i;
                pairs[k].loser = j;
                k++;
            }
            else if (preferences[i][j] < preferences[j][i])
            {
                pairs[k].winner = j;
                pairs[k].loser = i;
                k++;
            }
            else
            {
                continue;
            }
        }
    }
    pair_count = k;
    return;
}

void sort_pairs(void)
{
    int strength[pair_count];

    // Calculate the strengths and initialize an array of indices
    for (int i = 0; i < pair_count; i++)
    {
        strength[i] = preferences[pairs[i].winner][pairs[i].loser];
    }

    int sorted_indices[pair_count];
    for (int i = 0; i < pair_count; i++)
    {
        sorted_indices[i] = i;
    }

    // Sort the indices array based on the strengths using a simple bubble sort
    for (int i = 0; i < pair_count - 1; i++)
    {
        for (int j = 0; j < pair_count - i - 1; j++)
        {
            if (strength[sorted_indices[j]] < strength[sorted_indices[j + 1]])
            {
                int temp = sorted_indices[j];
                sorted_indices[j] = sorted_indices[j + 1];
                sorted_indices[j + 1] = temp;
            }
        }
    }

    // Create a copy of the pairs array
    pair pairs_copy[pair_count];
    for (int i = 0; i < pair_count; i++)
    {
        copy_pair(&pairs_copy[i], &pairs[i]);
    }

    // Rearrange the pairs array based on sorted indices
    for (int i = 0; i < pair_count; i++)
    {
        pairs[i] = pairs_copy[sorted_indices[i]];
    }

    // // Print the sorted pairs for verification
    // for (int i = 0; i < pair_count; i++)
    // {
    //     printf("Sorted pairs: (%i,%i)\n", pairs[i].winner, pairs[i].loser);
    // }
}

// Declare a function to perform a deep copy of a pair
void copy_pair(pair *dest, const pair *src)
{
    dest->winner = src->winner;
    dest->loser = src->loser;
}

bool forms_cycle(int winner, int loser, bool visited[], int n)
{
    visited[loser] = true;

    if (winner == loser)
    {
        return true;
    }

    for (int i = 0; i < n; i++)
    {
        if (i != loser && !visited[i] && locked[loser][i])
        {
            if (forms_cycle(winner, i, visited, n))
            {
                return true;
            }
        }
    }

    return false;
}

// Lock pairs into the candidate graph in order, without creating cycles
void lock_pairs(void)
{
    for (int i = 0; i < pair_count; i++)
    {
        int winner = pairs[i].winner;
        int loser = pairs[i].loser;

        // Create a copy of the locked matrix before trying the lock
        bool locked_copy[MAX][MAX];
        for (int j = 0; j < candidate_count; j++)
        {
            for (int k = 0; k < candidate_count; k++)
            {
                locked_copy[j][k] = locked[j][k];
            }
        }

        // Temporarily lock the current pair
        locked[winner][loser] = true;

        // Check if locking the pair forms a cycle
        bool visited[MAX];
        for (int j = 0; j < candidate_count; j++)
        {
            visited[j] = false;
        }

        if (!forms_cycle(winner, loser, visited, candidate_count))
        {
            // If no cycle is formed, keep the lock
            continue;
        }

        // If locking the pair forms a cycle, restore the original locked matrix
        for (int j = 0; j < candidate_count; j++)
        {
            for (int k = 0; k < candidate_count; k++)
            {
                locked[j][k] = locked_copy[j][k];
            }
        }
    }
}

void print_winner(void)
{
    // for (int i = 0; i < candidate_count; i++)
    // {
    //     for (int j = 0; j < candidate_count; j++)
    //     {
    //         printf("%i ", locked[i][j]);
    //     }
    //     printf("\n");
    // }

    for (int i = 0; i < candidate_count; i++)
    {
        bool is_source = true; // Assume candidate i is the source

        for (int j = 0; j < candidate_count; j++)
        {
            if (i != j && locked[j][i])
            {
                // If candidate j defeats candidate i, then i is not the source
                is_source = false;
                break;
            }
        }

        if (is_source)
        {
            printf("%s\n", candidates[i]); // Print the winner
            return;
        }
    }
}

