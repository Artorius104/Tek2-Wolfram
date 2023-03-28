##
## EPITECH PROJECT, 2022
## B-FUN-400-PAR-4-1-wolfram-gilbert.badiabo
## File description:
## Makefile
##

# BINARY_PATH 	:=	$(shell stack path --local-install-root)
BINARY_PATH 	=	.stack-work/install/x86_64-linux-tinfo6/*/8.10.7/bin/
PROJECT_NAME	=	wolfram-exe
NAME 			= 	wolfram

all:
	rm -rf $(NAME)
	stack build
	cp -r $(BINARY_PATH)/$(PROJECT_NAME) .
	mv $(PROJECT_NAME) $(NAME)

clean:
	stack clean

fclean:
	stack purge

re: fclean all

.PHONY: all clean fclean re