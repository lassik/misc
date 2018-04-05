#include <stdio.h>
#include <stdlib.h>

struct node {
    struct node *left;
    struct node *right;
    int val;
};

static int nextval;

static struct node *build(size_t depth)
{
    struct node *node;

    if(depth < 1) return(0);
    node = malloc(sizeof(struct node));
    node->val = nextval++;
    node->left = build(depth-1);
    node->right = build(depth-1);
    return(node);
}

static size_t depth(struct node *tree)
{
    size_t a, b;

    if(!tree) return(0);
    a = depth(tree->left);
    b = depth(tree->right);
    return(1 + ((a > b) ? a : b));
}

static void printdepth(struct node *node, size_t d, size_t gap)
{
    size_t i;

    if(d == 1) {
        printf("%02d", node->val);
        for(i=0; i<((size_t)2<<gap)-1; i++) printf("  ");
        return;
    }
    printdepth(node->left,  d-1, gap);
    printdepth(node->right, d-1, gap);
}

static void print(struct node *tree)
{
    size_t d, gap, i;

    for(d=1; d<=depth(tree); d++) {
        gap = depth(tree) - d;
        for(i=0; i<(1<<gap)-1; i++) printf("  ");
        printdepth(tree, d, gap);
        printf("\n");
    }
}

extern int main(void)
{
    print(build(6));
    return(0);
}
