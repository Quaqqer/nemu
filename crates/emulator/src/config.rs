pub struct NemuConfig {
    pub override_sprite_limit: bool,
}

impl Default for NemuConfig {
    fn default() -> Self {
        Self {
            override_sprite_limit: true,
        }
    }
}
