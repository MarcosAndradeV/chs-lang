type foo fn() -> *int
type Xs *int

type Dollar distinct int
type get_dollar fn() -> Dollar

fn main()
    x: *int = foo()
    y: Xs = foo()

    d: Dollar = get_dollar()
end
