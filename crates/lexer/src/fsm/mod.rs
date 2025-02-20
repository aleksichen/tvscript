// src/fsm/mod.rs
mod state;
mod processor;
mod transition;

pub use state::State;
pub use processor::StateProcessor;
pub use transition::TransitionHandler;
