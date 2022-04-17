use dim::array::Array;
use dim::eval::ranked_add;
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

#[test]
fn add_scalar_rank_zero() {
    k9::snapshot!(
        ranked_add(scalar(10).view(), scalar(5).view(), 0, 0).view().render(),
        "[]$[15]"
    );

    k9::snapshot!(
        ranked_add(scalar(10).view(), idot(vec![3]).view(), 0, 0)
            .view()
            .render(),
        "[3]$[10, 11, 12]"
    );
    k9::snapshot!(
        ranked_add(idot(vec![3]).view(), scalar(10).view(), 0, 0)
            .view()
            .render(),
        "[3]$[10, 11, 12]"
    );

    k9::snapshot!(
        ranked_add(scalar(10).view(), idot(vec![2, 3]).view(), 0, 0)
            .view()
            .render(),
        "[2, 3]$[10, 11, 12, 13, 14, 15]"
    );
}

#[test]
#[should_panic]
fn add_scalar_rank_one() {
    ranked_add(scalar(10).view(), scalar(5).view(), 1, 0);
}

#[test]
fn add_vector_rank_one() {
    k9::snapshot!(
        ranked_add(idot(vec![3]).view(), idot(vec![3]).view(), 1, 1)
            .view()
            .render(),
        "[3]$[0, 2, 4]"
    );
}

#[test]
#[should_panic]
fn add_vector_rank_one_length_mismatch() {
    ranked_add(idot(vec![2]).view(), idot(vec![3]).view(), 1, 1);
}

#[test]
fn add_scalar_rank_zero_vector_rank_one() {
    k9::snapshot!(
        ranked_add(scalar(10).view(), idot(vec![3]).view(), 0, 1)
            .view()
            .render(),
        "[3]$[10, 11, 12]"
    );
}

#[test]
fn add_vectors_rank_zero_one() {
    k9::snapshot!(
        ranked_add(idot(vec![2]).view(), idot(vec![3]).view(), 0, 1)
            .view()
            .render(),
        "[2, 3]$[0, 1, 2, 1, 2, 3]"
    );

    k9::snapshot!(
        ranked_add(idot(vec![2]).view(), idot(vec![3]).view(), 1, 0)
            .view()
            .render(),
        "[3, 2]$[0, 1, 1, 2, 2, 3]"
    );
}

#[test]
fn add_matrix_rank_zero() {
    k9::snapshot!(
        ranked_add(idot(vec![2, 3]).view(), idot(vec![2, 3]).view(), 0, 0)
            .view()
            .render(),
        "[2, 3]$[0, 2, 4, 6, 8, 10]"
    );
}

#[test]
fn add_matrix_rank_one() {
    k9::snapshot!(
        ranked_add(idot(vec![2, 3]).view(), idot(vec![2, 3]).view(), 1, 1)
            .view()
            .render(),
        "[2, 3]$[0, 2, 4, 6, 8, 10]"
    );
}

#[test]
fn add_vector_matrix_rank_zero() {
    k9::snapshot!(
        ranked_add(idot(vec![2]).view(), idot(vec![2, 3]).view(), 0, 0)
            .view()
            .render(),
        "[2, 3]$[0, 1, 2, 4, 5, 6]"
    );
}

#[test]
fn add_vector_matrix_rank_one() {
    k9::snapshot!(
        ranked_add(idot(vec![3]).view(), idot(vec![2, 3]).view(), 1, 1)
            .view()
            .render(),
        "[2, 3]$[0, 2, 4, 3, 5, 7]"
    );
}

#[test]
fn add_vector_matrix_rank_zero_one() {
    k9::snapshot!(
        ranked_add(idot(vec![2]).view(), idot(vec![2, 3]).view(), 0, 1)
            .view()
            .render(),
        "[2, 3]$[0, 1, 2, 4, 5, 6]"
    );

    k9::snapshot!(
        ranked_add(idot(vec![2]).view(), idot(vec![2, 3]).view(), 1, 0)
            .view()
            .render(),
        "[2, 3, 2]$[0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6]"
    );
}

#[test]
fn add_vector_matrix_rank_one_two() {
    k9::snapshot!(
        ranked_add(idot(vec![2]).view(), idot(vec![2, 3]).view(), 1, 2)
            .view()
            .render(),
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
