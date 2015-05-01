config.fish: ~/.config/fish/config.fish
	cp $< .

clean: config.fish
	rm $<
