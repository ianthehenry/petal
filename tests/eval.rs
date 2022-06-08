use petal::array::Array;
use petal::eval::ranked_add;
use k9;

// similar to J's i. function
fn idot(shape: Vec<usize>) -> Array<i64> {
    let data: Vec<i64> = (0..(shape.iter().product::<usize>() as i64)).collect();
    Array::<i64> { shape, data }
}
fn scalar(value: i64) -> Array<i64> {
    Array::<i64> {
        shape: vec![],
        data: vec![value],
    }
}

fn test_ranked_add(
    left: Array<i64>,
    right: Array<i64>,
    left_rank: isize,
    right_rank: isize,
) -> String {
    ranked_add(left.view(), right.view(), left_rank, right_rank)
        .view()
        .render()
}

#[test]
fn add_scalar_rank_zero() {
    k9::snapshot!(test_ranked_add(scalar(10), scalar(5), 0, 0), "[]$[15]");

    k9::snapshot!(
        test_ranked_add(scalar(10), idot(vec![3]), 0, 0),
        "[3]$[10, 11, 12]"
    );
    k9::snapshot!(
        test_ranked_add(idot(vec![3]), scalar(10), 0, 0),
        "[3]$[10, 11, 12]"
    );

    k9::snapshot!(
        test_ranked_add(scalar(10), idot(vec![2, 3]), 0, 0),
        "[2, 3]$[10, 11, 12, 13, 14, 15]"
    );
}

#[test]
#[should_panic]
fn add_scalar_rank_one() {
    test_ranked_add(scalar(10), scalar(5), 1, 0);
}

#[test]
fn add_vector_rank_one() {
    k9::snapshot!(
        test_ranked_add(idot(vec![3]), idot(vec![3]), 1, 1),
        "[3]$[0, 2, 4]"
    );
}

#[test]
#[should_panic]
fn add_vector_rank_one_length_mismatch() {
    test_ranked_add(idot(vec![2]), idot(vec![3]), 1, 1);
}

#[test]
fn add_scalar_rank_zero_vector_rank_one() {
    k9::snapshot!(
        test_ranked_add(scalar(10), idot(vec![3]), 0, 1),
        "[3]$[10, 11, 12]"
    );
}

#[test]
fn add_vectors_rank_zero_one() {
    k9::snapshot!(
        test_ranked_add(idot(vec![2]), idot(vec![3]), 0, 1),
        "[2, 3]$[0, 1, 2, 1, 2, 3]"
    );

    k9::snapshot!(
        test_ranked_add(idot(vec![2]), idot(vec![3]), 1, 0),
        "[3, 2]$[0, 1, 1, 2, 2, 3]"
    );
}

#[test]
fn add_matrix_rank_zero() {
    k9::snapshot!(
        test_ranked_add(idot(vec![2, 3]), idot(vec![2, 3]), 0, 0),
        "[2, 3]$[0, 2, 4, 6, 8, 10]"
    );
}

#[test]
fn add_matrix_rank_one() {
    k9::snapshot!(
        test_ranked_add(idot(vec![2, 3]), idot(vec![2, 3]), 1, 1),
        "[2, 3]$[0, 2, 4, 6, 8, 10]"
    );
}

#[test]
fn add_vector_matrix_rank_zero() {
    k9::snapshot!(
        test_ranked_add(idot(vec![2]), idot(vec![2, 3]), 0, 0),
        "[2, 3]$[0, 1, 2, 4, 5, 6]"
    );
}

#[test]
fn add_vector_matrix_rank_one() {
    k9::snapshot!(
        test_ranked_add(idot(vec![3]), idot(vec![2, 3]), 1, 1),
        "[2, 3]$[0, 2, 4, 3, 5, 7]"
    );
}

#[test]
fn add_vector_matrix_rank_zero_one() {
    k9::snapshot!(
        test_ranked_add(idot(vec![2]), idot(vec![2, 3]), 0, 1),
        "[2, 3]$[0, 1, 2, 4, 5, 6]"
    );

    k9::snapshot!(
        test_ranked_add(idot(vec![2]), idot(vec![2, 3]), 1, 0),
        "[2, 3, 2]$[0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6]"
    );
}

#[test]
fn add_vector_matrix_rank_one_two() {
    k9::snapshot!(
        test_ranked_add(idot(vec![2]), idot(vec![2, 3]), 1, 2),
        "[2, 3]$[0, 1, 2, 4, 5, 6]"
    );
}

#[test]
fn math() {
    use rug::{Assign, Integer, Rational};
    let mut int = Integer::new();
    int.assign(14);
    k9::snapshot!(int, "14");

    let mut rat = Rational::new();
    rat.assign((14, 3));
    k9::snapshot!(rat, "14/3");
}
