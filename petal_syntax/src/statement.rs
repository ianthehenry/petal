pub(super) type Block<T> = Vec<Statement<T>>;
pub(super) type Terms<T> = Vec<T>;

#[derive(Debug)]
pub(super) enum Statement<T> {
    SimpleAssignment(String, Terms<T>),
    CompoundAssignment(String, Block<T>),
    Expression(Terms<T>),
}
