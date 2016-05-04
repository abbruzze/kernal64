package ucesoft.c64.peripheral.keyboard

import java.awt.event.KeyEvent._
import CKey._

object DefaultKeyboardMapper extends KeyboardMapper {
  val cmap = Map(
    'ù' -> (EQUAL,false),
    '§' -> (EQUAL,true),
    'è' -> (STAR,false),
    'é' -> (EXP,true),
    'à' -> (SEMICOL,false),
    '°' -> (SEMICOL,true),
    'ò' -> (COLON,false),
    'ç' -> (COLON,true),
    'ì' -> (MINUS,false)
      )
  val map = Map(
    // row 0
    VK_BACK_SPACE -> INST_DEL,
    VK_ENTER -> RETURN,
    VK_RIGHT -> CRSR_LR,
    VK_F1 -> F1,
    VK_F3 -> F3,
    VK_F5 -> F5,
    VK_F7 -> F7,
    VK_DOWN -> CRSR_UD,
    // row 1
    VK_3 -> N_3,
    VK_W -> W,
    VK_A -> A,
    VK_4 -> N_4,
    VK_Z -> Z,
    VK_S -> S,
    VK_E -> E,
    VK_SHIFT -> L_SHIFT,
    // row 2
    VK_5 -> N_5,
    VK_R -> R,
    VK_D -> D,
    VK_6 -> N_6,
    VK_C -> C,
    VK_F -> F,
    VK_T -> T,
    VK_X -> X,
    // row 3
    VK_7 -> N_7,
    VK_Y -> Y,
    VK_G -> G,
    VK_8 -> N_8,
    VK_B -> B,
    VK_H -> H,
    VK_U -> U,
    VK_V -> V,
    // row 4
    VK_9 -> N_9,
    VK_I -> I,
    VK_J -> J,
    VK_0 -> N_0,
    VK_M -> M,
    VK_K -> K,
    VK_O -> O,
    VK_N -> N,
    // row 5
    VK_PLUS -> EXP,
    VK_P -> P,
    VK_L -> L,
    VK_PERIOD -> PERIOD,
    //VK_COLON -> COLON, with cmap
    VK_INSERT -> AT,
    VK_COMMA -> COMMA,
    // row 6
    VK_HOME -> CLR_HOME,
    VK_MINUS -> SLASH,
    // row 7
    VK_1 -> N_1,
    VK_BACK_SLASH -> L_ARROW,
    VK_CONTROL -> CTRL,
    VK_2 -> N_2,
    VK_SPACE -> SPACE,
    VK_LESS -> CBM,
    VK_Q -> Q,
    VK_ESCAPE -> RUN_STOP,
    VK_CAPS_LOCK -> RUN_STOP,
    VK_QUOTE -> PLUS,
    VK_DELETE -> RESTORE,
    VK_PAGE_UP -> LIRA)
}