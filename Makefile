##
## EPITECH PROJECT, 2024
## IMAGE COMPRESSOR
## File description:
## Makefile
##

NAM = mypandoc

all:
	stack build --copy-bins --local-bin-path . && \
	mv ./MyPandoc-exe ./$(NAM)

clean:
	stack clean

fclean: clean
	rm -f $(NAM)

re: fclean all

.PHONY: all clean fclean re
