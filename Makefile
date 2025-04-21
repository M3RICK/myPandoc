##
## EPITECH PROJECT, 2024
## myPandoc
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
