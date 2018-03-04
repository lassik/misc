// Check whether path.Ext() treats dotfiles specially.

package main

import (
	"fmt"
	"path"
	"runtime"
)

func demo(s string) {
	fmt.Printf("path.Ext(%q) == %q\n", s, path.Ext(s))
}

func main() {
	fmt.Printf("%s %s/%s\n", runtime.Version(), runtime.GOOS, runtime.GOARCH)
	fmt.Println("Normal filenames:")
	demo("name")
	demo("name.ext")
	demo("/name")
	demo("/name.ext")
	fmt.Println("Dotfile names:")
	demo(".name")
	demo(".name.ext")
	demo("/.name")
	demo("/.name.ext")
}
