profiles.clj: ~/.lein/profiles.clj
	cp $< .

clean:
	rm -f profiles.clj
