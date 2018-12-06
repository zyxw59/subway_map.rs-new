use std::borrow::Borrow;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash};

pub trait Table<Key: ?Sized, Value> {
    fn get(&self, key: &Key) -> Option<&Value>;
}

pub trait TableMut<Key, Value>: Table<Key, Value> {
    fn insert(&mut self, key: Key, value: Value) -> Option<Value>;
}

impl<K, Q, V, S> Table<Q, V> for HashMap<K, V, S>
where
    K: Borrow<Q> + Hash + Eq,
    Q: ?Sized + Hash + Eq,
    S: BuildHasher,
{
    fn get(&self, key: &Q) -> Option<&V> {
        HashMap::get(self, key)
    }
}

impl<K, V, S> TableMut<K, V> for HashMap<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    fn insert(&mut self, key: K, value: V) -> Option<V> {
        HashMap::insert(self, key, value)
    }
}

impl<K, V> Table<K, V> for () {
    fn get(&self, _key: &K) -> Option<&V> {
        None
    }
}

impl<'a, K, V, T: Table<K, V>> Table<K, V> for &'a T {
    fn get(&self, key: &K) -> Option<&V> {
        (*self).get(key)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{Table, TableMut};

    #[test]
    fn store_retrieve() {
        let key = String::from("x");
        let value = 10;
        let mut vars = HashMap::new();
        assert_eq!(TableMut::insert(&mut vars, key, value), None);
        assert_eq!(Table::get(&vars, "x"), Some(&10));
    }
}
