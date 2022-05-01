#[derive(Debug)]
pub struct Array<T> {
    pub shape: Vec<usize>,
    pub data: Vec<T>,
}

#[derive(Debug)]
pub struct ArrayView<'a, T> {
    pub shape: Vec<usize>,
    data: &'a [T],
}

#[derive(Debug)]
pub struct AtomIterator<'a, T> {
    index: usize,
    // the number of elements might be larger than the underlying data
    atom_count: usize,
    data: &'a [T],
}

#[derive(Debug, Clone)]
pub struct ArrayIterator<'a, T> {
    slice_shape: Vec<usize>,
    // this is just the cached product of the shape. should probably make a constructor...
    slice_length: usize,
    pub slice_count: usize, // TODO: probably shouldn't be public...
    index: usize,
    data: &'a [T],
}

impl<'a, T> Array<T> {
    pub fn view(&'a self) -> ArrayView<'a, T> {
        ArrayView {
            shape: self.shape.clone(),
            data: &self.data,
        }
    }
}

impl<'a, T> ArrayView<'a, T> {
    pub fn slice(&'a self, rank: usize) -> ArrayIterator<'a, T> {
        let frame = &self.shape[0..rank];
        let cells = &self.shape[rank..];
        ArrayIterator {
            index: 0,
            slice_shape: cells.to_vec(),
            slice_length: cells.iter().product(),
            slice_count: frame.iter().product(),
            data: self.data,
        }
    }

    pub fn reshape(&'a self, shape: &[usize]) -> ArrayView<'a, T> {
        ArrayView {
            shape: shape.to_vec(),
            data: self.data,
        }
    }

    pub fn iter(&'a self) -> AtomIterator<'a, T> {
        AtomIterator {
            index: 0,
            atom_count: self.shape.iter().product(),
            data: self.data,
        }
    }
}

impl<'a, T> IntoIterator for &'a ArrayView<'_, T> {
    type Item = &'a T;
    type IntoIter = AtomIterator<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> Iterator for ArrayIterator<'a, T> {
    type Item = ArrayView<'a, T>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.slice_count {
            let start = self.index * self.slice_length;
            let slice = ArrayView {
                shape: self.slice_shape.clone(),
                data: &self.data[start..start + self.slice_length],
            };
            self.index += 1;
            Some(slice)
        } else {
            None
        }
    }
}

impl<'a, T> Iterator for AtomIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.atom_count {
            let value = &self.data[self.index % self.data.len()];
            self.index += 1;
            Some(value)
        } else {
            None
        }
    }
}

impl<'a, T: std::fmt::Debug> ArrayView<'a, T> {
    pub fn render(&self) -> String {
        format!("{:?}${:?}", self.shape, self.iter().collect::<Vec<_>>())
    }
}
