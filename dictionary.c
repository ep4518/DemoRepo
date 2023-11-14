// Implements a dictionary's functionality

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <math.h>

#include "dictionary.h"

// Represents a node in a hash table
typedef struct node
{
    char word[LENGTH + 1];
    struct node *next;
} node;

// TODO:
const unsigned int N = 26;

// Hash table
node *table[N];

// Data Loaded
bool loaded = false;

int count_words = 0;

// Returns true if word is in dictionary, else false
bool check(const char *word)
{
    // TODO
    int index = hash(word);
    node *cursor = table[index];
    if (cursor == NULL)
    {
        return false;
    }
    while (cursor != NULL)
    {
        if (strcasecmp(word, cursor->word) == 0)
        {
            return true;
        }
        else
        {
            cursor = cursor->next;
        }
    }

    return false;
}

// Hashes word to a number
unsigned int hash(const char *word)
{
    // TODO: Hash Function
    return toupper(word[0]) - 'A';
}

bool load(const char *dictionary)
{
    // TODO
    FILE *file = fopen(dictionary, "r");
    if (file == NULL)
    {
        printf("Could not open %s\n", dictionary);
        return false;
    }
    else if (file != NULL)
    {
        char buffer[LENGTH + 1];
        while (fscanf(file, "%s", buffer) != EOF)
        {
            node *new = malloc(sizeof(node));

            if (new == NULL)
            {
                return false;
                break;
            }
            else if (new != NULL)
            {
                strcpy(new->word, buffer);
                new->next = NULL;

                // get the hash number
                int index = hash(buffer);

                if (table[index] == NULL)
                {
                    table[index] = new;
                }
                else if (table[index] != NULL)
                {
                    new->next = table[index];
                    table[index] = new;
                }
                count_words++;
            }
        }
        fclose(file);
        loaded = true;
        return true;
    }
    else
    {
        return false;
    }
}
// TODO
// Returns number of words in dictionary if loaded, else 0 if not yet loaded
unsigned int size(void)
{
    if (loaded)
    {
        return count_words;
    }
    else
    {
        return 0;
    }
}

// Unloads dictionary from memory, returning true if successful, else false
bool unload(void)
{
    for (int i = 0; i < N; i++)
    {
        node *cursor = table[i];
        while (cursor != NULL)
        {
            node *tmp = cursor;
            cursor = cursor->next;
            free(tmp);
        }

        table[i] = NULL; // Set table[i] to NULL after freeing the memory
    }
    return true;
}

