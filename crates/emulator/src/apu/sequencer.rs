pub(crate) struct Sequencer {
    sequence: u32,
    timer: u16,
    reload: u16,
    output: bool,
}

impl Sequencer {
    fn tick<F>(&mut self, enable: bool, f: F) -> bool
    where
        F: Fn(&mut u32),
    {
        if enable {
            if self.timer == 0 {
                self.timer = self.reload + 1;
                f(&mut self.sequence);
                self.output = self.sequence & 0b1 != 0;
            } else {
                self.timer -= 1;
            }
        }

        self.output
    }
}
