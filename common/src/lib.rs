extern crate rand;

use rand::{Rand, Rng, StdRng};

use std::fmt;
pub use std::collections::HashMap;

pub trait AllValues {
    fn all_values() -> Vec<Self>
    where
        Self: std::marker::Sized;
}

macro_rules! all_values_rand_impl {
    ($($t:ty)*) => ($(
        impl Rand for $t {
            fn rand<R: Rng>(rng: &mut R) -> Self {
                let values = Self::all_values();

                let len = values.len();

                if len == 0 {
                    panic!("Cannot pick a random value because T::all_values()\
 returned an empty vector!")
                } else {
                    let i = rng.gen_range(0, len);

                    values[i]
                }
            }
        }
    )*)
}

pub struct Platform {
    pub draw_poly: fn(f32, f32, usize, usize),
    pub draw_poly_with_matrix: fn([f32; 16], usize, usize),
    pub draw_textured_poly: fn(f32, f32, usize, TextureSpec, usize),
    pub draw_textured_poly_with_matrix: fn([f32; 16], usize, TextureSpec, usize),
    pub draw_text: fn(&str, (f32, f32), f32, f32, [f32; 4], usize),
    pub draw_layer: fn(usize, f32),
    pub set_verts: fn(Vec<Vec<f32>>),
}

pub struct State {
    pub rng: StdRng,
    pub cam_x: f32,
    pub cam_y: f32,
    pub zoom: f32,
    pub board: Board,
    pub mouse_pos: (f32, f32),
    pub mouse_held: bool,
    pub window_wh: (f32, f32),
    pub ui_context: UIContext,
    pub turn: Turn,
    pub deck: Vec<Card>,
    pub pile: Vec<Card>,
    pub player_hand: Vec<Card>,
    pub cpu_hands: Vec<Vec<Card>>,
    pub stashes: Stashes,
    pub hud_alpha: f32,
    pub highlighted: Highlighted,
    pub message: Message,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Turn {
    DrawUntilNumberCard,
    RevealHand(Participant),
    WhoStarts,
    FirstRound(StarterCards, Participant),
    FirstRoundPlayer(StarterCards),
    DrawInitialCard,
    SelectTurnOption,
    DrawThree,
    Grow,
    Spawn,
    Build,
    BuildSelect(Card, usize),
    Move,
    MoveSelect((i8, i8), usize, Piece),
    ConvertSlashDemolish,
    ConvertSlashDemolishDiscard((i8, i8), usize, Option<usize>, Option<usize>, Option<usize>),
    ConvertSlashDemolishWhich(
        (i8, i8),
        usize,
        Option<usize>,
        Option<usize>,
        Option<usize>,
        u8,
    ),
    ConvertSelect(
        (i8, i8),
        usize,
        Option<usize>,
        Option<usize>,
        Option<usize>,
        u8,
    ),
    Fly,
    FlySelectCarpet(Card, usize),
    FlySelect((i8, i8), Space, Card, usize),
    Hatch,
    HatchSelect(Card, usize),
    Discard,
    CpuTurn(Option<Participant>),
    Over(Participant, Option<Participant>),
}

pub enum Highlighted {
    NoHighlighting,
    PlayerOccupation,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Participant {
    Player,
    Cpu(usize),
}

pub struct Message {
    pub text: String,
    pub timeout: u32,
}

impl Default for Message {
    fn default() -> Message {
        Message {
            text: Default::default(),
            timeout: Default::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct StarterCards {
    pub player_card: Card,
    pub cpu_cards: [Option<Card>; MAX_PLAYERS - 1],
    pub first: Participant,
}

pub struct Stashes {
    pub player_stash: Stash,
    pub cpu_stashes: Vec<Stash>,
}

impl Index<PieceColour> for Stashes {
    type Output = Stash;

    fn index<'a>(&'a self, colour: PieceColour) -> &'a Stash {
        if self.player_stash.colour == colour {
            return &self.player_stash;
        } else {
            for stash in self.cpu_stashes.iter() {
                if stash.colour == colour {
                    return stash;
                }
            }

            panic!("Asked for a stash that wasn't there!");
        }
    }
}

impl IndexMut<PieceColour> for Stashes {
    fn index_mut<'a>(&'a mut self, colour: PieceColour) -> &'a mut Stash {
        if self.player_stash.colour == colour {
            return &mut self.player_stash;
        } else {
            for stash in self.cpu_stashes.iter_mut() {
                if stash.colour == colour {
                    return stash;
                }
            }

            panic!("Asked for a stash that wasn't there!");
        }
    }
}



#[derive(PartialEq, Debug)]
pub struct Stash {
    pub colour: PieceColour,
    pub one_pip: PiecesLeft,
    pub two_pip: PiecesLeft,
    pub three_pip: PiecesLeft,
}

impl Stash {
    pub fn add(&mut self, piece: Piece) {
        debug_assert!(
            self.colour == piece.colour,
            "Attempted to add a {:?} piece to a {:?} stash!",
            piece.colour,
            self.colour
        );

        self[piece.pips] = self[piece.pips].higher();
    }
    pub fn remove(&mut self, pips: Pips) -> Option<Piece> {
        if self[pips] == NoneLeft {
            None
        } else {
            self[pips] = self[pips].lower();

            Some(Piece {
                colour: self.colour,
                pips,
            })
        }
    }

    pub fn full(colour: PieceColour) -> Self {
        Stash {
            colour,
            one_pip: ThreeLeft,
            two_pip: ThreeLeft,
            three_pip: ThreeLeft,
        }
    }

    pub fn is_full(&self) -> bool {
        match self {
            &Stash {
                one_pip: ThreeLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
                ..
            } => true,
            _ => false,
        }
    }

    pub fn used_count(&self) -> u8 {
        STASH_MAX - u8::from(self.one_pip) - u8::from(self.two_pip) - u8::from(self.three_pip)
    }

    pub fn available_sizes_descending(&self) -> Vec<Pips> {
        let mut available_sizes = Pips::all_values();

        available_sizes.retain(|&pips| self[pips] != NoneLeft);

        available_sizes.sort_by_key(|&pips| match self[pips] {
            NoneLeft => 3,
            OneLeft => 2,
            TwoLeft => 1,
            ThreeLeft => 0,
        });

        available_sizes
    }
}

#[cfg(test)]
mod stash_tests {
    use ::*;

    #[test]
    fn remove() {
        let mut stash = Stash::full(PieceColour::Red);

        assert_eq!(stash[Pips::One], ThreeLeft);

        stash.remove(Pips::One);

        assert_eq!(stash[Pips::One], TwoLeft);

        stash.remove(Pips::One);

        assert_eq!(stash[Pips::One], OneLeft);

        stash.remove(Pips::One);

        assert_eq!(stash[Pips::One], NoneLeft);

        stash.remove(Pips::One);

        assert_eq!(stash[Pips::One], NoneLeft);
    }
}

pub const STASH_MAX: u8 = 9;

use std::ops::{Index, IndexMut};

impl Index<Pips> for Stash {
    type Output = PiecesLeft;

    fn index<'a>(&'a self, index: Pips) -> &'a PiecesLeft {
        match index {
            Pips::One => &self.one_pip,
            Pips::Two => &self.two_pip,
            Pips::Three => &self.three_pip,
        }
    }
}

impl IndexMut<Pips> for Stash {
    fn index_mut<'a>(&'a mut self, index: Pips) -> &'a mut PiecesLeft {
        match index {
            Pips::One => &mut self.one_pip,
            Pips::Two => &mut self.two_pip,
            Pips::Three => &mut self.three_pip,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum PiecesLeft {
    NoneLeft,
    OneLeft,
    TwoLeft,
    ThreeLeft,
}
use PiecesLeft::*;

impl PiecesLeft {
    fn higher(&self) -> PiecesLeft {
        match *self {
            NoneLeft => OneLeft,
            OneLeft => TwoLeft,
            TwoLeft | ThreeLeft => ThreeLeft,
        }
    }
    fn lower(&self) -> PiecesLeft {
        match *self {
            NoneLeft | OneLeft => NoneLeft,
            TwoLeft => OneLeft,
            ThreeLeft => TwoLeft,
        }
    }
}

impl From<PiecesLeft> for u8 {
    fn from(left: PiecesLeft) -> Self {
        match left {
            NoneLeft => 0,
            OneLeft => 1,
            TwoLeft => 2,
            ThreeLeft => 3,
        }
    }
}

pub type Board = HashMap<(i8, i8), Space>;

pub const INITIAL_WINDOW_WIDTH: u32 = 800;
pub const INITIAL_WINDOW_HEIGHT: u32 = 600;

pub const MAX_PLAYERS: usize = 5;
pub const MAX_PIECES_PER_PLAYER: usize = 9;
pub const MAX_PIECES_PER_SPACE: usize = MAX_PLAYERS * MAX_PIECES_PER_PLAYER;

#[derive(Copy)]
pub struct SpacePieces(pub [Option<Piece>; MAX_PIECES_PER_SPACE]);

//All `None`s are assumed to be at the end of the array
impl SpacePieces {
    pub fn remove(&mut self, index: usize) -> Option<Piece> {
        if index >= MAX_PIECES_PER_SPACE {
            return None;
        }

        if let Some(piece) = self.0[index] {
            for i in index..MAX_PIECES_PER_SPACE - 1 {
                if self.0[i].is_none() {
                    break;
                }

                self.0[i] = self.0[i + 1];
            }

            Some(piece)
        } else {
            None
        }
    }

    pub fn push(&mut self, piece: Piece) {
        let len = self.len();
        if len < MAX_PIECES_PER_SPACE {
            self.0[len] = Some(piece);
        }
    }

    pub fn len(&self) -> usize {
        for i in 0..MAX_PIECES_PER_SPACE {
            if self.0[i].is_none() {
                return i;
            }
        }

        MAX_PIECES_PER_SPACE
    }

    pub fn insert(&mut self, index: usize, piece: Piece) {
        if index >= MAX_PIECES_PER_SPACE {
            return;
        }

        if self.0[index].is_some() {
            for i in (index..MAX_PIECES_PER_SPACE - 1).rev() {
                if self.0[i].is_none() {
                    continue;
                }

                self.0[i + 1] = self.0[i];
            }
        }

        self.0[index] = Some(piece)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Piece> {
        if index >= MAX_PIECES_PER_SPACE {
            return None;
        }

        match self.0[index] {
            Some(ref mut piece) => Some(piece),
            None => None,
        }
    }

    pub fn get(&self, index: usize) -> Option<Piece> {
        if index >= MAX_PIECES_PER_SPACE {
            return None;
        }

        self.0[index]
    }

    pub fn any<F>(&self, f: F) -> bool
    where
        F: Fn(Piece) -> bool,
    {
        for i in 0..MAX_PIECES_PER_SPACE {
            if let Some(piece) = self.0[i] {
                if f(piece) {
                    return true;
                }
            } else {
                break;
            }
        }

        false
    }

    pub fn filtered_indicies<F>(&self, f: F) -> Vec<usize>
    where
        F: Fn(Piece) -> bool,
    {
        let mut result = Vec::new();

        for i in 0..MAX_PIECES_PER_SPACE {
            if let Some(piece) = self.0[i] {
                if f(piece) {
                    result.push(i);
                }
            }
        }

        result
    }
}



impl fmt::Debug for SpacePieces {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}
impl Default for SpacePieces {
    fn default() -> Self {
        SpacePieces([None; MAX_PIECES_PER_SPACE])
    }
}

impl Clone for SpacePieces {
    fn clone(&self) -> SpacePieces {
        SpacePieces((*self).0)
    }
}

impl PartialEq for SpacePieces {
    fn eq(&self, other: &SpacePieces) -> bool {
        for i in 0..MAX_PIECES_PER_SPACE {
            if self.0[i] != other.0[i] {
                return false;
            }
        }

        true
    }
}

//TODO could use a non-consuming one if the copy causes slowness
// https://stackoverflow.com/a/30220832/4496839
impl IntoIterator for SpacePieces {
    type Item = Piece;
    type IntoIter = SpacePiecesIterator;

    fn into_iter(self) -> Self::IntoIter {
        SpacePiecesIterator {
            pieces: self.0,
            index: 0,
        }
    }
}

pub struct SpacePiecesIterator {
    pieces: [Option<Piece>; MAX_PIECES_PER_SPACE],
    index: usize,
}

impl Iterator for SpacePiecesIterator {
    type Item = Piece;
    fn next(&mut self) -> Option<Piece> {
        if self.index >= MAX_PIECES_PER_SPACE {
            return None;
        }

        let result = self.pieces[self.index];
        self.index += 1;

        result
    }
}

#[derive(PartialEq, Copy, Clone, Debug, Default)]
pub struct Space {
    pub card: Card,
    pub pieces: SpacePieces,
    pub offset: u8,
}

impl Space {
    pub fn new(card: Card) -> Self {
        Space {
            card,
            ..Default::default()
        }
    }
}
impl Rand for Space {
    fn rand<R: Rng>(rng: &mut R) -> Self {
        let mut pieces = SpacePieces::default();

        //TODO remove u32 step which was just to maintain the same
        //set of random results
        //see https://github.com/rust-lang-nursery/rand/issues/164
        for i in 0..(rng.gen_range::<u32>(0, 5) as usize) {
            pieces.0[i] = Some(rng.gen());
        }

        Space {
            card: rng.gen(),
            pieces,
            offset: 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Piece {
    pub colour: PieceColour,
    pub pips: Pips,
}

impl AllValues for Piece {
    fn all_values() -> Vec<Piece> {
        let mut deck = Vec::new();

        for &colour in PieceColour::all_values().iter() {
            for &pips in Pips::all_values().iter() {
                deck.push(Piece { colour, pips });
            }
        }

        deck
    }
}

all_values_rand_impl!(Piece);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PieceColour {
    Red,
    Yellow,
    Green,
    Blue,
    Black,
}
use PieceColour::*;

impl AllValues for PieceColour {
    fn all_values() -> Vec<PieceColour> {
        vec![Red, Yellow, Green, Blue, Black]
    }
}

all_values_rand_impl!(PieceColour);

impl From<PieceColour> for f32 {
    fn from(colour: PieceColour) -> Self {
        match colour {
            Red => 0.0,
            Yellow => 1.0,
            Green => 2.0,
            Blue => 3.0,
            Black => 0.0,
        }
    }
}
impl From<PieceColour> for i32 {
    fn from(colour: PieceColour) -> Self {
        match colour {
            Red => 0,
            Yellow => 0,
            Green => 0,
            Blue => 0,
            Black => 1,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Pips {
    One,
    Two,
    Three,
}

impl Pips {
    pub fn higher(&self) -> Self {
        match *self {
            Pips::One => Pips::Two,
            _ => Pips::Three,
        }
    }
}

impl AllValues for Pips {
    fn all_values() -> Vec<Pips> {
        vec![Pips::One, Pips::Two, Pips::Three]
    }
}

all_values_rand_impl!(Pips);

impl From<Pips> for u8 {
    fn from(pips: Pips) -> Self {
        match pips {
            Pips::One => 1,
            Pips::Two => 2,
            Pips::Three => 3,
        }
    }
}

use std::cmp::Ordering;

impl Ord for Pips {
    fn cmp(&self, other: &Pips) -> Ordering {
        u8::from(*self).cmp(&u8::from(*other))
    }
}

impl PartialOrd for Pips {
    fn partial_cmp(&self, other: &Pips) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub type TextureSpec = (f32, f32, f32, f32, i32, f32, f32, f32, f32);


#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Card {
    pub suit: Suit,
    pub value: Value,
}

impl Default for Card {
    fn default() -> Card {
        Card {
            suit: Spades,
            value: Ace,
        }
    }
}

impl Card {
    pub fn is_number(&self) -> bool {
        self.value.is_number()
    }
}

impl AllValues for Card {
    fn all_values() -> Vec<Card> {
        let mut deck = Vec::new();

        for &suit in Suit::all_values().iter() {
            for &value in Value::all_values().iter() {
                deck.push(Card {
                    suit: suit,
                    value: value,
                });
            }
        }

        deck
    }
}

all_values_rand_impl!(Card);

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} of {}", self.value, self.suit)
    }
}

//texture size in pixels
//(that is, the side lingth of the square texture)
pub const T_S: f32 = 1024.0;

//in pixels
pub const CARD_TEXTURE_PIXEL_WIDTH: f32 = 140.0;
pub const CARD_TEXTURE_PIXEL_HEIGHT: f32 = 190.0;

//in the range 0 - 1
pub const CARD_TEXTURE_WIDTH: f32 = CARD_TEXTURE_PIXEL_WIDTH / T_S;
pub const CARD_TEXTURE_HEIGHT: f32 = CARD_TEXTURE_PIXEL_HEIGHT / T_S;

pub const TOOLTIP_TEXTURE_PIXEL_WIDTH: f32 = 75.0;
pub const TOOLTIP_TEXTURE_PIXEL_HEIGHT: f32 = 26.0;
pub const TOOLTIP_TEXTURE_WIDTH: f32 = TOOLTIP_TEXTURE_PIXEL_WIDTH / T_S;
pub const TOOLTIP_TEXTURE_HEIGHT: f32 = TOOLTIP_TEXTURE_PIXEL_HEIGHT / T_S;

pub const TOOLTIP_TEXTURE_HEIGHT_OFFSET: f32 = (TOOLTIP_TEXTURE_PIXEL_HEIGHT + 1.0) / T_S;



impl Card {
    pub fn texture_spec(&self) -> TextureSpec {
        let mut x = f32::from(self.value);
        let mut texture_index = 0;

        if x >= 7.0 {
            x -= 7.0;
            texture_index = 1;
        }

        (
            x * CARD_TEXTURE_WIDTH,
            f32::from(self.suit) * CARD_TEXTURE_HEIGHT,
            CARD_TEXTURE_WIDTH,
            CARD_TEXTURE_HEIGHT,
            texture_index,
            0.0,
            0.0,
            0.0,
            0.0,
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Suit {
    Clubs,
    Diamonds,
    Hearts,
    Spades,
}
use Suit::*;

impl fmt::Display for Suit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Clubs => "♣".to_string(),
                Diamonds => "♦".to_string(),
                Hearts => "♥".to_string(),
                Spades => "♠".to_string(),
            }
        )
    }
}

impl AllValues for Suit {
    fn all_values() -> Vec<Suit> {
        vec![Clubs, Diamonds, Hearts, Spades]
    }
}

impl From<Suit> for f32 {
    fn from(suit: Suit) -> Self {
        match suit {
            Clubs => 0.0,
            Diamonds => 1.0,
            Hearts => 2.0,
            Spades => 3.0,
        }
    }
}

impl Ord for Suit {
    fn cmp(&self, other: &Suit) -> Ordering {
        (f32::from(*self) as u8).cmp(&(f32::from(*other) as u8))
    }
}

impl PartialOrd for Suit {
    fn partial_cmp(&self, other: &Suit) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Value {
    Ace,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
}
use Value::*;

impl Value {
    pub fn is_number(&self) -> bool {
        match *self {
            Jack | Queen | King | Ace => false,
            _ => true,
        }
    }

    pub fn number_value(&self) -> u8 {
        match *self {
            Jack | Queen | King | Ace => 0,
            Two => 2,
            Three => 3,
            Four => 4,
            Five => 5,
            Six => 6,
            Seven => 7,
            Eight => 8,
            Nine => 9,
            Ten => 10,
        }
    }

    pub fn lower_number(&self) -> Option<Value> {
        match *self {
            Two | Ace | Queen | King => None,
            Three => Some(Two),
            Four => Some(Three),
            Five => Some(Four),
            Six => Some(Five),
            Seven => Some(Six),
            Eight => Some(Seven),
            Nine => Some(Eight),
            Ten => Some(Nine),
            Jack => Some(Ten),
        }
    }

    pub fn higher_number(&self) -> Option<Value> {
        match *self {
            Ten | Jack | Queen | King => None,
            Ace => Some(Two),
            Two => Some(Three),
            Three => Some(Four),
            Four => Some(Five),
            Five => Some(Six),
            Six => Some(Seven),
            Seven => Some(Eight),
            Eight => Some(Nine),
            Nine => Some(Ten),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Ace => "A".to_string(),
                Two => "2".to_string(),
                Three => "3".to_string(),
                Four => "4".to_string(),
                Five => "5".to_string(),
                Six => "6".to_string(),
                Seven => "7".to_string(),
                Eight => "8".to_string(),
                Nine => "9".to_string(),
                Ten => "10".to_string(),
                Jack => "J".to_string(),
                Queen => "Q".to_string(),
                King => "K".to_string(),
            }
        )
    }
}

impl AllValues for Value {
    fn all_values() -> Vec<Value> {
        vec![
            Ace,
            Two,
            Three,
            Four,
            Five,
            Six,
            Seven,
            Eight,
            Nine,
            Ten,
            Jack,
            Queen,
            King,
        ]
    }
}

impl From<Value> for f32 {
    fn from(value: Value) -> Self {
        match value {
            Ace => 0.0,
            Two => 1.0,
            Three => 2.0,
            Four => 3.0,
            Five => 4.0,
            Six => 5.0,
            Seven => 6.0,
            Eight => 7.0,
            Nine => 8.0,
            Ten => 9.0,
            Jack => 10.0,
            Queen => 11.0,
            King => 12.0,
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Value) -> Ordering {
        (f32::from(*self) as u8).cmp(&(f32::from(*other) as u8))
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub enum Event {
    Quit,
    KeyDown(Keycode),
    KeyUp(Keycode),
    MouseMove((i32, i32)),
    LeftMouseUp,
    LeftMouseDown,
    RightMouseUp,
    RightMouseDown,
    WindowSize((i32, i32)),
}

#[derive(Clone, Copy, Debug)]
pub struct ProjectionSpec {
    pub top: f32,
    pub bottom: f32,
    pub left: f32,
    pub right: f32,
    pub near: f32,
    pub far: f32,
    pub projection: Projection,
}

impl ProjectionSpec {
    pub fn inverse(&self) -> Self {
        ProjectionSpec {
            top: self.top,
            bottom: self.bottom,
            left: self.left,
            right: self.right,
            near: self.near,
            far: self.far,
            projection: self.projection.inverse(),
        }
    }
}



#[derive(Clone, Copy, Debug)]
pub enum Projection {
    Perspective,
    InversePerspective,
    Orthographic,
    InverseOrthographic,
}
use Projection::*;

impl AllValues for Projection {
    fn all_values() -> Vec<Projection> {
        vec![
            Perspective,
            InversePerspective,
            Orthographic,
            InverseOrthographic,
        ]
    }
}

all_values_rand_impl!(Projection);

impl Projection {
    pub fn inverse(&self) -> Self {
        match *self {
            Perspective => InversePerspective,
            InversePerspective => Perspective,
            Orthographic => InverseOrthographic,
            InverseOrthographic => Orthographic,
        }
    }
}

// see http://www.scratchapixel.com perspecitve and orthographic projection sections

//inverses found through wolframalpha, accounting for the openGL transposition
// invert {
//{(2*n)/(r-l),0,0,0},
//{0,(2*n)/(t-b),0,0},
//{(r+l)/(r-l),(t+b)/(t-b),-(f+n)/(f-n),-1},
//{0,0,-(2*f * n)/(f-n),0}
//}

// invert {
//{(2)/(r-l),0,0,0},
//{0,2/(t-b),0,0},
//{0,0,-2/(f-n),0},
//{-(r + l)/(r - l),-(t+b)/(t-b),-(f + n)/(f-n),1}
//}

pub fn get_projection(spec: &ProjectionSpec) -> [f32; 16] {
    match spec.projection {
        Perspective => {
            [
                (2.0 * spec.near) / (spec.right - spec.left),
                0.0,
                (spec.right + spec.left) / (spec.right - spec.left),
                0.0,
                //
                0.0,
                (2.0 * spec.near) / (spec.top - spec.bottom),
                (spec.top + spec.bottom) / (spec.top - spec.bottom),
                0.0,
                //
                0.0,
                0.0,
                -(spec.far + spec.near) / (spec.far - spec.near),
                -(2.0 * spec.far * spec.near) / (spec.far - spec.near),
                //
                0.0,
                0.0,
                -1.0,
                0.0,
            ]
        }
        InversePerspective => {
            [
                (spec.right - spec.left) / (2.0 * spec.near),
                0.0,
                0.0,
                (spec.right + spec.left) / (2.0 * spec.near),
                //
                0.0,
                (spec.top - spec.bottom) / (2.0 * spec.near),
                0.0,
                (spec.top + spec.bottom) / (2.0 * spec.near),
                //
                0.0,
                0.0,
                0.0,
                -1.0,
                //
                0.0,
                0.0,
                (spec.near - spec.far) / (2.0 * spec.far * spec.near),
                (spec.near + spec.far) / (2.0 * spec.far * spec.near),
            ]
        }
        Orthographic => {
            [
                2.0 / (spec.right - spec.left),
                0.0,
                0.0,
                -(spec.right + spec.left) / (spec.right - spec.left),
                //
                0.0,
                2.0 / (spec.top - spec.bottom),
                0.0,
                -(spec.top + spec.bottom) / (spec.top - spec.bottom),
                //
                0.0,
                0.0,
                -2.0 / (spec.far - spec.near),
                -(spec.far + spec.near) / (spec.far - spec.near),
                //
                0.0,
                0.0,
                0.0,
                1.0,
            ]
        }
        InverseOrthographic => {
            [
                (spec.right - spec.left) / 2.0,
                0.0,
                0.0,
                (spec.left + spec.right) / 2.0,
                //
                0.0,
                (spec.top - spec.bottom) / 2.0,
                0.0,
                (spec.top + spec.bottom) / 2.0,
                //
                0.0,
                0.0,
                (spec.near - spec.far) / 2.0,
                -(spec.far + spec.near) / 2.0,
                //
                0.0,
                0.0,
                0.0,
                1.0,
            ]
        }
    }
}

pub fn card_id((x, y): (i8, i8)) -> UiId {
    10_000 + (x as UiId + 1000) * 1000 + y as UiId + x as UiId
}

pub fn piece_id(card_id: UiId, offset: u8) -> UiId {
    (card_id + 30_000 + (offset as UiId * 500) * 1000) as _
}

pub fn arrow_id(card_id: UiId, forward: bool) -> UiId {
    card_id * 2 + 10_000 - if forward { 1 } else { 0 }
}

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod id_tests {
    use ::*;

    quickcheck! {
        fn uniqueness(coords: (i8, i8), offset: u8, forward: bool) -> bool {
            let card_id = card_id(coords);
            let piece_id = piece_id(card_id, offset);
            let arrow_id = arrow_id(card_id, forward);

            (card_id != piece_id) && (piece_id != arrow_id) && (card_id != arrow_id)
        }
    }
}

use std::collections::HashSet;

#[cfg(test)]
mod fly_check_tests {
    use ::*;

    #[test]
    fn minimal() {
        let mut board = HashMap::new();

        board.insert((0, 0), Default::default());

        let targets = fly_from_targets(&board, &(0, 0));

        assert_eq!(targets, vec![]);
    }

    #[test]
    fn all() {
        let mut board = HashMap::new();

        let one_up: (i8, i8) = (0, 1);

        board.insert((0, 0), Default::default());
        board.insert(one_up, Default::default());

        let mut targets = fly_from_targets(&board, &one_up);

        let mut expected: Vec<_> = EIGHT_WAY_OFFSETS
            .iter()
            .filter(|o| *o != &one_up)
            .cloned()
            .collect();

        targets.sort();
        expected.sort();

        assert_eq!(targets, expected);
    }

    #[test]
    fn some_not_all() {
        let mut board = HashMap::new();

        board.insert((0, -1), Default::default());
        board.insert((0, 0), Default::default());
        board.insert((0, 1), Default::default());

        let mut targets = fly_from_targets(&board, &(0, 0));

        let mut expected = vec![(-1, 0), (1, 0)];

        targets.sort();
        expected.sort();

        assert_eq!(targets, expected);
    }
}

pub fn is_space_movable(board: &Board, from: &(i8, i8)) -> bool {
    fly_from_targets(board, from).len() > 0
}

pub fn fly_from_targets(board: &Board, from: &(i8, i8)) -> Vec<(i8, i8)> {
    let targets = get_all_diagonally_connected_empty_spaces(board);

    let mut result = Vec::new();

    let mut remaining_spaces: HashSet<_> = board.keys().collect();

    remaining_spaces.remove(from);

    for target in targets {
        if can_fly(&remaining_spaces, &target) {
            result.push(target);
        }
    }

    result.sort();

    result
        .iter()
        .filter_map(|k| if k == from { None } else { Some(*k) })
        .collect()
}

fn can_fly(remaining_spaces: &HashSet<&(i8, i8)>, to: &(i8, i8)) -> bool {
    if remaining_spaces.contains(to) || remaining_spaces.len() == 0 {
        return false;
    }

    let mut seen = HashSet::new();

    can_fly_helper(&remaining_spaces, &mut seen, to, to)
}

fn can_fly_helper(
    remaining_spaces: &HashSet<&(i8, i8)>,
    seen: &mut HashSet<(i8, i8)>,
    current: &(i8, i8),
    to: &(i8, i8),
) -> bool {
    if seen.contains(&current) {
        return false;
    }

    if remaining_spaces.contains(&current) || current == to {
        seen.insert(*current);
    } else {
        return false;
    }

    //we're done when seen contains `to` and everything in `remaining_spaces`
    if seen.len() > remaining_spaces.len() {
        return true;
    }

    for &(x, y) in EIGHT_WAY_OFFSETS.iter() {
        if can_fly_helper(
            remaining_spaces,
            seen,
            &(x.saturating_add(current.0), y.saturating_add(current.1)),
            to,
        ) {
            return true;
        }
    }

    false
}

pub const FOUR_WAY_OFFSETS: [(i8, i8); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

pub const EIGHT_WAY_OFFSETS: [(i8, i8); 8] = [
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
    (-1, -1),
    (0, -1),
    (1, -1),
];

pub fn get_all_diagonally_connected_empty_spaces(board: &Board) -> HashSet<(i8, i8)> {
    let filled_coords = board.keys();

    let mut result = HashSet::new();

    for &(x, y) in filled_coords {
        for &(dx, dy) in EIGHT_WAY_OFFSETS.iter() {
            let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
            if !board.contains_key(&new_coords) {
                result.insert(new_coords);
            }
        }
    }

    result
}

#[cfg(test)]
mod card_tests {
    use ::*;

    #[test]
    fn sort_by_suit_then_value() {
        let mut cards = vec![
            Card {
                suit: Hearts,
                value: Two,
            },
            Card {
                suit: Clubs,
                value: Three,
            },
            Card {
                suit: Clubs,
                value: Two,
            },
        ];

        cards.sort();

        assert_eq!(
            cards,
            vec![
                Card {
                    suit: Clubs,
                    value: Two,
                },
                Card {
                    suit: Clubs,
                    value: Three,
                },
                Card {
                    suit: Hearts,
                    value: Two,
                },
            ]
        )
    }
}

#[cfg(test)]
mod mat4x4_tests {
    use ::*;

    impl Rand for ProjectionSpec {
        fn rand<R: Rng>(rng: &mut R) -> Self {
            ProjectionSpec {
                top: rng.gen(),
                bottom: rng.gen(),
                left: rng.gen(),
                right: rng.gen(),
                near: rng.gen(),
                far: rng.gen(),
                projection: rng.gen(),
            }
        }
    }

    impl quickcheck::Arbitrary for ProjectionSpec {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> ProjectionSpec {
            g.gen()
        }
    }

    #[derive(Clone, Copy, Debug)]
    struct Mat4x4 {
        m: [f32; 16],
    }

    impl Rand for Mat4x4 {
        fn rand<R: Rng>(rng: &mut R) -> Self {
            let m = [
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
                rng.gen(),
            ];

            Mat4x4 { m }
        }
    }

    impl quickcheck::Arbitrary for Mat4x4 {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Mat4x4 {
            g.gen()
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    quickcheck! {
        fn projection_inversion(mat4x4: Mat4x4, spec_: ProjectionSpec) -> bool {
            let m = mat4x4.m;

            let mut spec = spec_.clone();
            spec.projection = Orthographic;
            let p = get_projection(&spec);
            let p_inv = get_projection(&spec.inverse());

            let actual = mat4x4_mul(&p_inv, &mat4x4_mul(&p,&m));

            // m ~= actual
            let mut error:f32 = 0.0;
            for i in 0..actual.len() {
                let current_error = (actual[i] - m[i]).abs();
                error = if error > current_error {
                    error
                } else {
                    current_error
                };
            }

            error <= 0.0001
        }

        fn projection_camera_inversion(
            mat4x4: Mat4x4,
            spec_: ProjectionSpec,
            cam_x: f32,
            cam_y:f32
        )
            -> bool
        {
            let m = mat4x4.m;

            let mut spec = spec_.clone();
            spec.projection = Orthographic;
            let p = get_projection(&spec);
            let p_inv = get_projection(&spec.inverse());

            let camera = [
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                cam_x,
                cam_y,
                0.0,
                1.0,
            ];

            let camera_inv = [
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                -cam_x,
                -cam_y,
                0.0,
                1.0,
            ];

            let view = mat4x4_mul(&camera, &p);
            let view_inv = mat4x4_mul(&p_inv, &camera_inv);

            let actual = mat4x4_mul(&view_inv, &mat4x4_mul(&view,&m));

            // m ~= actual
            let mut error:f32 = 0.0;
            for i in 0..actual.len() {
                let current_error = (actual[i] - m[i]).abs();
                error = if error > current_error {
                    error
                } else {
                    current_error
                };
            }
            println!("error {}", error);
            error <= 0.25
        }
    }

}

pub fn mat4x4_mul(a: &[f32; 16], b: &[f32; 16]) -> [f32; 16] {
    [
        a[0] * b[0] + a[1] * b[4] + a[2] * b[8] + a[3] * b[12],
        a[0] * b[1] + a[1] * b[5] + a[2] * b[9] + a[3] * b[13],
        a[0] * b[2] + a[1] * b[6] + a[2] * b[10] + a[3] * b[14],
        a[0] * b[3] + a[1] * b[7] + a[2] * b[11] + a[3] * b[15],
        a[4] * b[0] + a[5] * b[4] + a[6] * b[8] + a[7] * b[12],
        a[4] * b[1] + a[5] * b[5] + a[6] * b[9] + a[7] * b[13],
        a[4] * b[2] + a[5] * b[6] + a[6] * b[10] + a[7] * b[14],
        a[4] * b[3] + a[5] * b[7] + a[6] * b[11] + a[7] * b[15],
        a[8] * b[0] + a[9] * b[4] + a[10] * b[8] + a[11] * b[12],
        a[8] * b[1] + a[9] * b[5] + a[10] * b[9] + a[11] * b[13],
        a[8] * b[2] + a[9] * b[6] + a[10] * b[10] + a[11] * b[14],
        a[8] * b[3] + a[9] * b[7] + a[10] * b[11] + a[11] * b[15],
        a[12] * b[0] + a[13] * b[4] + a[14] * b[8] + a[15] * b[12],
        a[12] * b[1] + a[13] * b[5] + a[14] * b[9] + a[15] * b[13],
        a[12] * b[2] + a[13] * b[6] + a[14] * b[10] + a[15] * b[14],
        a[12] * b[3] + a[13] * b[7] + a[14] * b[11] + a[15] * b[15],
    ]
}

pub fn mat4x4_vector_mul(
    m: &[f32; 16],
    v_0: f32,
    v_1: f32,
    v_2: f32,
    v_3: f32,
) -> (f32, f32, f32, f32) {
    (
        m[0] * v_0 + m[4] * v_1 + m[8] * v_2 + m[12] * v_3,
        m[1] * v_0 + m[5] * v_1 + m[9] * v_2 + m[13] * v_3,
        m[2] * v_0 + m[6] * v_1 + m[10] * v_2 + m[14] * v_3,
        m[3] * v_0 + m[7] * v_1 + m[11] * v_2 + m[15] * v_3,
    )
}
pub fn mat4x4_vector_mul_divide(
    m: &[f32; 16],
    v_0: f32,
    v_1: f32,
    v_2: f32,
    v_3: f32,
) -> (f32, f32, f32, f32) {
    let (x, y, z, w) = mat4x4_vector_mul(m, v_0, v_1, v_2, v_3);
    (x / w, y / w, z / w, 1.0)
}

pub type UiId = i32;

pub struct UIContext {
    pub hot: UiId,
    pub active: UiId,
    pub next_hot: UiId,
}

impl UIContext {
    pub fn new() -> Self {
        UIContext {
            hot: 0,
            active: 0,
            next_hot: 0,
        }
    }

    pub fn set_not_active(&mut self) {
        self.active = 0;
    }
    pub fn set_active(&mut self, id: UiId) {
        self.active = id;
    }
    pub fn set_next_hot(&mut self, id: UiId) {
        self.next_hot = id;
    }
    pub fn set_not_hot(&mut self) {
        self.hot = 0;
    }
    pub fn frame_init(&mut self) {
        if self.active == 0 {
            self.hot = self.next_hot;
        }
        self.next_hot = 0;
    }
}



//combined from https://github.com/AngryLawyer/rust-sdl2/blob/master/sdl2-sys/src/keycode.rs
// and https://github.com/AngryLawyer/rust-sdl2/blob/master/src/sdl2/keyboard/keycode.rs
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[repr(i32)]
pub enum Keycode {
    Backspace = 8 as i32,
    Tab = 9 as i32,
    Return = 13 as i32,
    Escape = 27 as i32,
    Space = 32 as i32,
    Exclaim = 33 as i32,
    Quotedbl = 34 as i32,
    Hash = 35 as i32,
    Dollar = 36 as i32,
    Percent = 37 as i32,
    Ampersand = 38 as i32,
    Quote = 39 as i32,
    LeftParen = 40 as i32,
    RightParen = 41 as i32,
    Asterisk = 42 as i32,
    Plus = 43 as i32,
    Comma = 44 as i32,
    Minus = 45 as i32,
    Period = 46 as i32,
    Slash = 47 as i32,
    Num0 = 48 as i32,
    Num1 = 49 as i32,
    Num2 = 50 as i32,
    Num3 = 51 as i32,
    Num4 = 52 as i32,
    Num5 = 53 as i32,
    Num6 = 54 as i32,
    Num7 = 55 as i32,
    Num8 = 56 as i32,
    Num9 = 57 as i32,
    Colon = 58 as i32,
    Semicolon = 59 as i32,
    Less = 60 as i32,
    Equals = 61 as i32,
    Greater = 62 as i32,
    Question = 63 as i32,
    At = 64 as i32,
    LeftBracket = 91 as i32,
    Backslash = 92 as i32,
    RightBracket = 93 as i32,
    Caret = 94 as i32,
    Underscore = 95 as i32,
    Backquote = 96 as i32,
    A = 97 as i32,
    B = 98 as i32,
    C = 99 as i32,
    D = 100 as i32,
    E = 101 as i32,
    F = 102 as i32,
    G = 103 as i32,
    H = 104 as i32,
    I = 105 as i32,
    J = 106 as i32,
    K = 107 as i32,
    L = 108 as i32,
    M = 109 as i32,
    N = 110 as i32,
    O = 111 as i32,
    P = 112 as i32,
    Q = 113 as i32,
    R = 114 as i32,
    S = 115 as i32,
    T = 116 as i32,
    U = 117 as i32,
    V = 118 as i32,
    W = 119 as i32,
    X = 120 as i32,
    Y = 121 as i32,
    Z = 122 as i32,
    Delete = 127 as i32,
    CapsLock = 1073741881 as i32,
    F1 = 1073741882 as i32,
    F2 = 1073741883 as i32,
    F3 = 1073741884 as i32,
    F4 = 1073741885 as i32,
    F5 = 1073741886 as i32,
    F6 = 1073741887 as i32,
    F7 = 1073741888 as i32,
    F8 = 1073741889 as i32,
    F9 = 1073741890 as i32,
    F10 = 1073741891 as i32,
    F11 = 1073741892 as i32,
    F12 = 1073741893 as i32,
    PrintScreen = 1073741894 as i32,
    ScrollLock = 1073741895 as i32,
    Pause = 1073741896 as i32,
    Insert = 1073741897 as i32,
    Home = 1073741898 as i32,
    PageUp = 1073741899 as i32,
    End = 1073741901 as i32,
    PageDown = 1073741902 as i32,
    Right = 1073741903 as i32,
    Left = 1073741904 as i32,
    Down = 1073741905 as i32,
    Up = 1073741906 as i32,
    NumLockClear = 1073741907 as i32,
    KpDivide = 1073741908 as i32,
    KpMultiply = 1073741909 as i32,
    KpMinus = 1073741910 as i32,
    KpPlus = 1073741911 as i32,
    KpEnter = 1073741912 as i32,
    Kp1 = 1073741913 as i32,
    Kp2 = 1073741914 as i32,
    Kp3 = 1073741915 as i32,
    Kp4 = 1073741916 as i32,
    Kp5 = 1073741917 as i32,
    Kp6 = 1073741918 as i32,
    Kp7 = 1073741919 as i32,
    Kp8 = 1073741920 as i32,
    Kp9 = 1073741921 as i32,
    Kp0 = 1073741922 as i32,
    KpPeriod = 1073741923 as i32,
    Application = 1073741925 as i32,
    Power = 1073741926 as i32,
    KpEquals = 1073741927 as i32,
    F13 = 1073741928 as i32,
    F14 = 1073741929 as i32,
    F15 = 1073741930 as i32,
    F16 = 1073741931 as i32,
    F17 = 1073741932 as i32,
    F18 = 1073741933 as i32,
    F19 = 1073741934 as i32,
    F20 = 1073741935 as i32,
    F21 = 1073741936 as i32,
    F22 = 1073741937 as i32,
    F23 = 1073741938 as i32,
    F24 = 1073741939 as i32,
    Execute = 1073741940 as i32,
    Help = 1073741941 as i32,
    Menu = 1073741942 as i32,
    Select = 1073741943 as i32,
    Stop = 1073741944 as i32,
    Again = 1073741945 as i32,
    Undo = 1073741946 as i32,
    Cut = 1073741947 as i32,
    Copy = 1073741948 as i32,
    Paste = 1073741949 as i32,
    Find = 1073741950 as i32,
    Mute = 1073741951 as i32,
    VolumeUp = 1073741952 as i32,
    VolumeDown = 1073741953 as i32,
    KpComma = 1073741957 as i32,
    KpEqualsAS400 = 1073741958 as i32,
    AltErase = 1073741977 as i32,
    Sysreq = 1073741978 as i32,
    Cancel = 1073741979 as i32,
    Clear = 1073741980 as i32,
    Prior = 1073741981 as i32,
    Return2 = 1073741982 as i32,
    Separator = 1073741983 as i32,
    Out = 1073741984 as i32,
    Oper = 1073741985 as i32,
    ClearAgain = 1073741986 as i32,
    CrSel = 1073741987 as i32,
    ExSel = 1073741988 as i32,
    Kp00 = 1073742000 as i32,
    Kp000 = 1073742001 as i32,
    ThousandsSeparator = 1073742002 as i32,
    DecimalSeparator = 1073742003 as i32,
    CurrencyUnit = 1073742004 as i32,
    CurrencySubUnit = 1073742005 as i32,
    KpLeftParen = 1073742006 as i32,
    KpRightParen = 1073742007 as i32,
    KpLeftBrace = 1073742008 as i32,
    KpRightBrace = 1073742009 as i32,
    KpTab = 1073742010 as i32,
    KpBackspace = 1073742011 as i32,
    KpA = 1073742012 as i32,
    KpB = 1073742013 as i32,
    KpC = 1073742014 as i32,
    KpD = 1073742015 as i32,
    KpE = 1073742016 as i32,
    KpF = 1073742017 as i32,
    KpXor = 1073742018 as i32,
    KpPower = 1073742019 as i32,
    KpPercent = 1073742020 as i32,
    KpLess = 1073742021 as i32,
    KpGreater = 1073742022 as i32,
    KpAmpersand = 1073742023 as i32,
    KpDblAmpersand = 1073742024 as i32,
    KpVerticalBar = 1073742025 as i32,
    KpDblVerticalBar = 1073742026 as i32,
    KpColon = 1073742027 as i32,
    KpHash = 1073742028 as i32,
    KpSpace = 1073742029 as i32,
    KpAt = 1073742030 as i32,
    KpExclam = 1073742031 as i32,
    KpMemStore = 1073742032 as i32,
    KpMemRecall = 1073742033 as i32,
    KpMemClear = 1073742034 as i32,
    KpMemAdd = 1073742035 as i32,
    KpMemSubtract = 1073742036 as i32,
    KpMemMultiply = 1073742037 as i32,
    KpMemDivide = 1073742038 as i32,
    KpPlusMinus = 1073742039 as i32,
    KpClear = 1073742040 as i32,
    KpClearEntry = 1073742041 as i32,
    KpBinary = 1073742042 as i32,
    KpOctal = 1073742043 as i32,
    KpDecimal = 1073742044 as i32,
    KpHexadecimal = 1073742045 as i32,
    LCtrl = 1073742048 as i32,
    LShift = 1073742049 as i32,
    LAlt = 1073742050 as i32,
    LGui = 1073742051 as i32,
    RCtrl = 1073742052 as i32,
    RShift = 1073742053 as i32,
    RAlt = 1073742054 as i32,
    RGui = 1073742055 as i32,
    Mode = 1073742081 as i32,
    AudioNext = 1073742082 as i32,
    AudioPrev = 1073742083 as i32,
    AudioStop = 1073742084 as i32,
    AudioPlay = 1073742085 as i32,
    AudioMute = 1073742086 as i32,
    MediaSelect = 1073742087 as i32,
    Www = 1073742088 as i32,
    Mail = 1073742089 as i32,
    Calculator = 1073742090 as i32,
    Computer = 1073742091 as i32,
    AcSearch = 1073742092 as i32,
    AcHome = 1073742093 as i32,
    AcBack = 1073742094 as i32,
    AcForward = 1073742095 as i32,
    AcStop = 1073742096 as i32,
    AcRefresh = 1073742097 as i32,
    AcBookmarks = 1073742098 as i32,
    BrightnessDown = 1073742099 as i32,
    BrightnessUp = 1073742100 as i32,
    DisplaySwitch = 1073742101 as i32,
    KbdIllumToggle = 1073742102 as i32,
    KbdIllumDown = 1073742103 as i32,
    KbdIllumUp = 1073742104 as i32,
    Eject = 1073742105 as i32,
    Sleep = 1073742106 as i32,
}
