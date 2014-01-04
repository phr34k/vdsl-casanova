module RTSgame.RTS

type Operation = Add = 0 | Subtract = 1
type Condition = Equal = 0 | NotEqual = 1

type ResourcesAttribute() =
  class
  end

type ActionAttribute() =
  class
    member val Target : string = null with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type RestrictAttribute() =
  class
    member val Condition : Condition = Condition.NotEqual with get,set
    member val Field : string = null with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type TransferAttribute() =
  class
    member val Operation : Operation = Operation.Add with get,set
    member val From : string = null with get,set
    member val To : string = null with get,set
    member val Multiplier : float32 = 1.0f with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type ThresholdAttribute() =
  class
    member val Field    : string = null with get,set
    member val Value    : string = null with get,set
  end

type RadiusAttribute(radius:float32) =
  class
  end
