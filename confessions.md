# Confessions

**READER BEWARE: THIS FILE CONTAINS SPOILERS FOR SOME OF THE PROBLEMS IN ADVENT OF CODE.
THEY ARE PLACED INSIDE** `details` **TAGS BUT YOUR BROWSER MAY NOT SUPPORT THIS, LEAVING THEM VISIBLE.
READ WITH CAUTION.**

While all of the code in this repository is written by me, I cannot claim that all solutions presented are entirely my own.
What do I mean by this?
If I felt particularly stuck with a challenge, I would do a very smalla mount of research on other peoples solutions, typically on the [Advent of Code subreddit](https://old.reddit.com/r/adventofcode).
While doing so, I would avoid looking at actual code unless I *absolutley* needed to.
Instead, I tried to only read written descriptions of how other solutions worked to see I there was a piece of the puzzle I had overlooked.
I maintain that I only did this to attempt to see the problems from different angles, that I might understand them better.
Below are the problems that I have researched and what I learned from doing so.

<details>
	<summary>2015 - Day 7 Part 1</summary>
	My first major leap in progress came after I realised I could use a similar approach for this problem as I did for 2019 Day 6.
	After I implemented the hash in Ruby I ran the program, but it seemed to be taking longer than it should.
	I suspected that I was dealing with circular dependancies, as the input data is essentially a directional graph.
	I was very frustrated at this and decided to search for answers when I came across the old Daily Solutions Megathread on the subreddit.
	There I learned that after calculating a wire's value, I could replace the expression to calculate in the hash with the result.
	I implemented this and it worked.
	After doing some more research after the fact, it seems that I was not the only person who ran into this problem.
</details>

