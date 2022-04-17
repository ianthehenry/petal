use super::array::{Array, ArrayView};

fn get_frame(shape: &[usize], rank: isize) -> &[usize] {
    let actual_rank = shape.len();
    let min_desired_rank = rank.abs() as usize;
    if actual_rank < min_desired_rank {
        // J actually pads with 1s, but I feel like that's more likely to
        // silently do the wrong thing? I feel like I'd want it to fail early
        // here. I wonder if J pads differently with a negative and a positive
        // rank.
        panic!("not enough ranks")
    }

    let cell_size = if rank < 0 {
        actual_rank - (-rank as usize)
    } else {
        rank as usize
    };

    &shape[0..actual_rank - cell_size]
}

fn reassemble(arrays: Vec<Array<i64>>, frame: &[usize]) -> Array<i64> {
    println!("reassembling {:?}\n  frame {:?}\n", arrays, frame);
    if let Some(first_value) = arrays.first() {
        let expected_shape = first_value.shape.clone();
        let mut assembled_data: Vec<i64> =
            Vec::with_capacity(expected_shape.iter().product::<usize>() * arrays.len());

        for mut array in arrays {
            if array.shape != expected_shape {
                panic!("mismatched shapes while reassembling results")
            }
            assembled_data.append(&mut array.data);
        }
        let mut result_shape = frame.to_vec();
        result_shape.extend(expected_shape);
        Array {
            shape: result_shape,
            data: assembled_data,
        }
    } else {
        // TODO: what does this case mean?
        Array {
            shape: frame.to_vec(),
            data: vec![],
        }
    }
}

fn prim_add(left: ArrayView<i64>, right: ArrayView<i64>) -> Array<i64> {
    // we could check here that they are both scalars, and only add if they are.
    // in some sense that's more correct. but this is more general and causes us
    // to recurse slightly less, but i dunno if it's worth it.
    if left.shape != right.shape {
        panic!("prim_add called with incompatible shapes");
    }

    let result_values = left.iter().zip(right.iter()).map(|(x, y)| x + y).collect();

    Array {
        shape: left.shape.clone(),
        data: result_values,
    }
}

pub fn ranked_add(
    left: ArrayView<i64>,
    right: ArrayView<i64>,
    left_rank: isize,
    right_rank: isize,
) -> Array<i64> {
    println!("{:?}:{:?} + {:?}:{:?}", left_rank, left, right_rank, right);

    let left_frame = get_frame(&left.shape, left_rank);
    let right_frame = get_frame(&right.shape, right_rank);

    let (common_frame, full_frame) = if left_frame.starts_with(right_frame) {
        (right_frame, left_frame)
    } else if right_frame.starts_with(left_frame) {
        (left_frame, right_frame)
    } else {
        panic!("incompatible shapes")
    };

    let left_surplus_frame = &left_frame[common_frame.len()..];
    let right_surplus_frame = &right_frame[common_frame.len()..];
    let surplus_frame = &full_frame[common_frame.len()..];

    println!("  left frame: {:?}+{:?}", common_frame, left_surplus_frame);
    println!(
        "  right frame: {:?}+{:?}",
        common_frame, right_surplus_frame
    );

    let left_macrocells = left.slice(common_frame.len());
    let right_macrocells = right.slice(common_frame.len());

    println!("  left macrocells: {:?}", left_macrocells);
    println!("  right macrocells: {:?}", right_macrocells);

    assert!(left_macrocells.slice_count == right_macrocells.slice_count);

    // TODO: we reassemble twice here, which means we copy the data twice.
    // can we do it all in one go? i'm not sure -- i'm not sure how rank
    // mismatches would behave.
    let results = left_macrocells
        .zip(right_macrocells)
        .map(|(left_macrocell, right_macrocell)| {
            let left_cells = left_macrocell.slice(left_surplus_frame.len());
            let right_cells = right_macrocell.slice(right_surplus_frame.len());

            println!("  left cells: {:?}", left_cells);
            println!("  right cells: {:?}", right_cells);
            // because only one side can have a surplus frame
            assert!(left_cells.slice_count == 1 || right_cells.slice_count == 1);
            let cell_count = std::cmp::max(left_cells.slice_count, right_cells.slice_count);
            let pairs = left_cells.cycle().zip(right_cells.cycle()).take(cell_count);

            let results = pairs
                .map(|(left, right)| {
                    println!("  paired");
                    println!("    left: {:?}\n    right: {:?}", left, right);

                    // TODO: this is a little weird. really we want to ask if
                    // the ranks of the values match the "native" ranks of the
                    // add operator, and apply the operation if so.
                    if left.shape == right.shape {
                        prim_add(left, right)
                    } else {
                        ranked_add(left, right, 0, 0)
                    }
                })
                .collect::<Vec<_>>();
            reassemble(results, surplus_frame)
        })
        .collect::<Vec<_>>();
    reassemble(results, common_frame)
}
