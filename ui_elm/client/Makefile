# This is the makefile for the elm code to produce the user interface
# for the CPC-DAQ

ELM_SRC=src/main.elm
SERVER_PUBLIC=Public
FLAGS=  --warn --output

main.js:
	@echo "Starting compilation of main file..."
	@elm-make $(ELM_SRC) $(FLAGS) $(SERVER_PUBLIC)/js/$@
	@cp index.html $(SERVER_PUBLIC)/.
	@cp app.css $(SERVER_PUBLIC)/.

clean:
	@echo "Removing current html and js files..."
	rm -rf $(SERVER_PUBLIC)/index.html
	rm -rf $(SERVER_PUBLIC)/main.js