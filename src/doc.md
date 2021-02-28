## orr
Võtab argumendiks massiivi samatüübilisi vidinaid ja tagastab esimesena tagastanu.

## and
Võtab argumendiks massiivi samatüübilisi vidinaid ja tagastab siis kui kõik on tagastanud.

Pooleliolev vidin on vidin.

Vidin võib muuhulgas tagastada vidina või funktsiooni mis teeb vidina.


## struct

Võtab argumendiks vidinate objekti ja tagastab nende lisaväärtused


```purescript

button :: String -> Widget Unit


onoff = 
    state False 
        [ True <$ button "on"
        , False <$ button "off"
        ]

labelOnOff text =
    
```


