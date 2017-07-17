// Helps find and remove stacks of empty directories.
// Prints 'rmdir' commands that can be fed to the shell.
//
// $ go run emptydirs.go
// rmdir "foo/bar/baz"
// rmdir "foo/bar"
// rmdir "foo/qux"
// rmdir "foo"
// rmdir "froz"

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"path"
	"strconv"
)

func walk(dirname string) bool {
	isEmpty := true
	infos, err := ioutil.ReadDir(dirname)
	if err != nil {
		log.Fatal(err)
	}
	for _, info := range infos {
		if !info.IsDir() || !walk(path.Join(dirname, info.Name())) {
			isEmpty = false
		}
	}
	if isEmpty {
		fmt.Println("rmdir", strconv.Quote(dirname))
	}
	return isEmpty
}

func main() {
	walk(".")
}
