# Wordle Solver

> In process

# How to Use this Program

`wordle-solver` is a command line program that captures the word guesses a player enters and gives you a list of the words matching the guesses.

Let's play a game:

## 1st Guess: "ATONE"

![First Guess](/doc/playedGames/Light1.jpg)

The game is showing that the 'T' is present in the correct word, but that it will ***not*** be found in column 2.

Hence, modify the code in `src/Main.purs` as follows:

```haskel
userInput :: UserInput
userInput = UserInput
  { plays:
      """
      ATONE
      """
  , found: "....."
  , other:
      [ Tuple 2 'T'
      ]
  }
```

Run the program in the terminal:

```bash
spago run > doc/playedGames/output1.txt
```

Out of the almost 5,000 words in the available 5-char per word list, only 165 are candidates.

[output1.txt](doc/playedGames/output1.txt)


## 2nd Guess: "BUILD"

![Second Guess](/doc/playedGames/Light2.jpg)

Add the "BUILD" to the `plays` and the tuples for the 'I' that is ***not*** in column 3 and the 'L' that is ***not*** in column 4.

Hence, modify the code in `src/Main.purs` as follows:

```haskel
userInput :: UserInput
userInput = UserInput
  { plays:
      """
      ATONE
      BUILD
      """
  , found: "....."
  , other:
      [ Tuple 2 'T'
      , Tuple 3 'I'
      , Tuple 4 'L'
      ]
  }
```

Run the program in the terminal:

```bash
spago run > doc/playedGames/output2.txt
```

Out of the almost 5,000 words in the available 5-char per word list, now only 15 are candidates.

[output2.txt](doc/playedGames/output1.txt)

## 3rd Guess: "SPLIT"

![Third Guess](/doc/playedGames/Light3.jpg)

When scanning the output, I noticed that "SPLIT" was the only word that didn't have an "I" in the 2nd column so I guessed that one.  So I modified the code to

1. Add "SPLIT" to `plays`.
1. Add two more tuples indicating that 'I' and 'L' are not in their respective positions either.
1. And, this is new, specify that the answer word has a 'T' in its column 5.

> Note: 'T' is reassigned to a fix position when "SPLIT" is added.  This effectively replaces the assignment as an `other` when the first word "ATONE" was added.  You don't have to remove the `Tuple 2 'T' from the `other` field.

Hence, modify the code in `src/Main.purs` as follows:

```haskel
userInput :: UserInput
userInput = UserInput
  { plays:
      """
      ATONE
      BUILD
      SPLIT
      """
  , found: "....T"
  , other:
      [ Tuple 2 'T'
      , Tuple 3 'I'
      , Tuple 4 'L'
      ]
  }
```

Run the program in the terminal:

```bash
spago run > doc/playedGames/output3.txt
```

Out of the almost 5,000 words in the available 5-char per word list, now only 3 are candidates.

[output3.txt](doc/playedGames/output3.txt)

## 4th Guess: "LIGHT"

There are only 3 possibilities left.

My gut said that "LIGHT" is the right answer, so I specified that. And, lo, it was a lucky guess.

![Fourth Guess](/doc/playedGames/Light4.jpg)



The puzzle is solved.
