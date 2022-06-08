pub(super) type Block<T> = Vec<Statement<T>>;
pub(super) type Expression<T> = Vec<T>;

#[derive(Debug)]
pub(super) enum Statement<T> {
    SimpleAssignment(String, Expression<T>),
    CompoundAssignment(String, Block<T>),
    Expression(Expression<T>),
}
