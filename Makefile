config.fish: ~/.config/fish/config.fish
	cp $< .

clean:
	rm -f config.fish
