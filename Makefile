##
## EPITECH PROJECT, 2025
## Bootstrap Wolfram
## File description:
## Makefile
##

NAME 	=	Glados-On-Top-exe

AT_NAME =	glados

build: 		stack
			cp $(shell stack path --local-install-root)/bin/$(NAME) .
			mv $(NAME) $(AT_NAME)

all: 		build

clean:
			stack clean

fclean: 	clean
			stack clean --full
			rm -f $(AT_NAME)

re: 		fclean all

tests_run:
			stack test

coverage:
			stack test --coverage

stack:
			stack build