pub trait RhsVisitable<I> {
    /// visits the given object,
    /// runs till the right most lowest item, calls `function` on that with the second
    /// parameter being [None]
    /// takes that result and moves back up, on every step it calls the function again,
    /// with the result from the previous child
    fn visit_rhs_mod_travel_up<F, R>(&mut self, function: &F) -> R
    where
        F: Fn(&mut I, Option<R>) -> R;
}

#[cfg(test)]
mod test {
    use super::*;
    #[derive(Debug, PartialEq, Eq)]
    struct TreeNode {
        value: &'static str,
        lhs: Option<Box<TreeNode>>,
        rhs: Option<Box<TreeNode>>,
    }

    impl TreeNode {
        fn new(
            value: &'static str,
            lhs: Option<Box<TreeNode>>,
            rhs: Option<Box<TreeNode>>,
        ) -> Box<Self> {
            Box::new(Self { value, lhs, rhs })
        }
    }

    impl RhsVisitable<Self> for TreeNode {
        fn visit_rhs_mod_travel_up<F, R>(&mut self, function: &F) -> R
        where
            F: Fn(&mut Self, Option<R>) -> R,
        {
            if self.rhs.is_none() {
                function(self, None)
            } else {
                let child_res = self.rhs.as_mut().unwrap().visit_rhs_mod_travel_up(function);
                function(self, Some(child_res))
            }
        }
    }

    fn fixture() -> Box<TreeNode> {
        TreeNode::new(
            "root",
            Some(TreeNode::new("left", None, None)),
            Some(TreeNode::new(
                "right1",
                None,
                Some(TreeNode::new("right2", None, None)),
            )),
        )
    }

    #[test]
    fn test_rhs_visit() {
        let mut tree = fixture();
        let f = |item: &mut TreeNode, child_res| {
            match child_res {
                None => assert_eq!("right2", item.value),
                Some("right2") => {
                    assert_eq!("right1", item.value);
                }
                Some("right1") => {
                    assert_eq!("root", item.value);
                }
                Some(v) => panic!("Unexpected child name {:#?}", v),
            }
            item.value
        };
        assert_eq!("root", tree.visit_rhs_mod_travel_up(&f));
    }

    #[test]
    fn test_rhs_visit_mut() {
        let mut tree = fixture();
        let f = |item: &mut TreeNode, _| item.value = "a";
        let expected = TreeNode::new(
            "a",
            Some(TreeNode::new("left", None, None)),
            Some(TreeNode::new(
                "a",
                None,
                Some(TreeNode::new("a", None, None)),
            )),
        );
        tree.visit_rhs_mod_travel_up(&f);
        assert_eq!(expected, tree);
    }
}
