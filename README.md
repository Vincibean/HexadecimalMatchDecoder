# Hexadecimal Match Decoder

Match data is streamed each time a team scores. 
The data contains the match time when the points were scored, an indication of which team scored and the current match score. 
The data is received using an hexadecimal representations. 
This utility:
- parses incoming data into a data structure suitable for capturing the current state of the match
- maintains the match state as additional data items are received. 

Moreover, the provided data structure provides a way to query the match state for the following:
- the last event (i.e. which team last scored, how many points, at what point through the match and what the resulting match score was)
- the last n events (where 0 <= n <= Total Items)
- all events in the match so far

Finally, this utility  handles cases where an invalid event is received or where the new event is inconsistent with 
previous events received.