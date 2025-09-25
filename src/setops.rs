// Copyright 2025 Oxide Computer Company

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct SetCompare<K, V> {
    pub a_unique: BTreeMap<K, V>,
    pub common: BTreeMap<K, (V, V)>,
    pub b_unique: BTreeMap<K, V>,
}

impl<K, V> SetCompare<K, V> {
    pub fn new<I, I2>(a: I, b: I2) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        I2: IntoIterator<Item = (K, V)>,
        K: Ord,
    {
        let mut aa = a.into_iter().collect::<BTreeMap<_, _>>().into_iter();
        let mut bb = b.into_iter().collect::<BTreeMap<_, _>>().into_iter();

        let mut compare = Self {
            a_unique: Default::default(),
            common: Default::default(),
            b_unique: Default::default(),
        };

        let mut a_entry = aa.next();
        let mut b_entry = bb.next();

        loop {
            match (a_entry, b_entry) {
                (None, None) => break,

                (Some((k, v)), None) => {
                    compare.a_unique.insert(k, v);
                    compare.a_unique.extend(aa);
                    break;
                }
                (None, Some((k, v))) => {
                    compare.b_unique.insert(k, v);
                    compare.b_unique.extend(bb);
                    break;
                }

                (Some((ak, av)), Some((bk, bv))) => match ak.cmp(&bk) {
                    std::cmp::Ordering::Less => {
                        compare.a_unique.insert(ak, av);
                        a_entry = aa.next();
                        b_entry = Some((bk, bv));
                    }
                    std::cmp::Ordering::Greater => {
                        compare.b_unique.insert(bk, bv);
                        a_entry = Some((ak, av));
                        b_entry = bb.next();
                    }
                    std::cmp::Ordering::Equal => {
                        compare.common.insert(ak, (av, bv));
                        a_entry = aa.next();
                        b_entry = bb.next();
                    }
                },
            }
        }

        compare
    }
}

#[cfg(test)]
mod tests {

    use crate::setops::SetCompare;

    #[test]
    fn test_cmp() {
        let a = [(1, "a"), (2, "b")];
        let b = [(1, "aa"), (3, "c")];

        let cmp = SetCompare::new(a, b);

        assert_eq!(cmp.a_unique.len(), 1);
        assert_eq!(cmp.common.len(), 1);
        assert_eq!(cmp.b_unique.len(), 1);

        assert_eq!(cmp.a_unique.get(&2).unwrap(), &"b");
        assert_eq!(cmp.b_unique.get(&3).unwrap(), &"c");
        assert_eq!(cmp.common.get(&1).unwrap(), &("a", "aa"));
    }
}
