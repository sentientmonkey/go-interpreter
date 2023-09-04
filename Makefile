monkey:
	go build

repl:
	go run main.go

test:
	go test  ./...

clean:
	rm -f monkey

.PHONY: test repl clean
