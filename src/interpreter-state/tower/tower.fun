structure Tower : TOWER =
struct


type phase = int

type 'a t = 'a option DynamicArray.array

type 'a createNew = 'a t * phase -> 'a

exception InvalidPhase
  
fun mkTower () = DynamicArray.array(1, NONE)

fun get (tower, phase) =
  if phase < 0 then (
    raise InvalidPhase
  ) else (
    DynamicArray.sub(tower, phase)
  )
    
fun set(tower, phase, phaseState) =
  if phase < 0 then (
    raise InvalidPhase
  ) else (
    DynamicArray.update(tower, phase, SOME phaseState)
  )


end
