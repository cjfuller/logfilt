package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"

	"github.com/fatih/color"
)

type colorNode struct {
	c        *color.Color
	text     string
	children *[]*colorNode
}

var deleteRegexes = []*regexp.Regexp{
	regexp.MustCompile("recording\\.py"),
	regexp.MustCompile("GET /gae_mini_profiler"),
	regexp.MustCompile("Stripped prohibited headers from URLFetch request: \\['Host'\\]"),
	regexp.MustCompile("Mismatch between XSRF header \\(None\\) and cookie"),
}

var expressionsToDelete = []*regexp.Regexp{
	regexp.MustCompile("\\d{4}-\\d{2}-\\d{2}\\s*(\\d{2}:\\d{2}:\\d{2})[^]]*]"),
}

var colorRegexes = map[*regexp.Regexp](*color.Color){
	regexp.MustCompile("^.*!!!.*$"):                   color.New(color.FgCyan),
	regexp.MustCompile("^.*---.*$"):                   color.New(color.FgCyan),
	regexp.MustCompile("\\d{4}-\\d{2}-\\d{2}[^]]*]/"): color.New(color.FgHiBlack),
	regexp.MustCompile("INFO"):                        color.New(color.FgGreen),
	regexp.MustCompile("WARNING"):                     color.New(color.FgYellow),
	regexp.MustCompile("ERROR"):                       color.New(color.FgRed),
	regexp.MustCompile("CRITICAL"):                    color.New(color.FgMagenta),
}

func shouldDeleteLine(line string) bool {
	for _, re := range deleteRegexes {
		if re.MatchString(line) {
			return true
		}
	}
	return false
}

func deleteExpressions(line string) string {
	for _, re := range expressionsToDelete {
		line = re.ReplaceAllString(line, "$1")
	}
	return line
}

func colorize(node *colorNode) *colorNode {
	if node.children != nil {
		for _, child := range *node.children {
			colorize(child)
		}
		return node
	}

	for key, value := range colorRegexes {
		if key.MatchString(node.text) {
			parts := key.Split(node.text, 2)
			matchText := key.FindString(node.text)
			if parts[0] != "" || parts[1] != "" {
				children := []*colorNode{
					&colorNode{c: node.c, text: parts[0], children: nil},
					&colorNode{c: value, text: matchText, children: nil},
					&colorNode{c: node.c, text: parts[1], children: nil},
				}
				node.children = &children
				return colorize(node)
			}
			node.c = value
		}
	}
	return node
}

func (node colorNode) toString() string {
	if node.children == nil {
		return node.c.Sprint(node.text)
	}

	result := ""
	for _, child := range *node.children {
		result += child.toString()
	}
	return result
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		if shouldDeleteLine(line) {
			continue
		}
		line = deleteExpressions(line)
		colorExpr := colorNode{
			c:        color.New(color.Reset),
			text:     line,
			children: nil,
		}
		colorized := colorize(&colorExpr)
		fmt.Println(colorized.toString())
	}
}
