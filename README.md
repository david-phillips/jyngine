## j y n g i n e: a minimalist json selector language.

jyngine is the selector language.  
jyngine.pl is the swi prolog implementation.


say you have this json:

    {
        "name": "Miles Davis Quintet",
    
        "members": [
            { "name": "Miles Davis",    "instr": "trumpet"   },
            { "name": "Wayne Shorter",  "instr": "tenor sax" },
            { "name": "Herbie Hancock", "instr": "piano"     },
            { "name": "Ron Carter",     "instr": "bass"      },
            { "name": "Tony Williams",  "instr": "drums"     }
        ]
    }

<table>
<tr><th>you want</th><th>you ask</th><th>you get</th></tr>
<tr><td>get the band name</td><td>@.name</td><td>Miles Davis Quintet</td></tr>
<tr><td>get the leader</td><td>@.members[0]</td><td>{"name":"Miles Davis", "instr":"trumpet"}</td></tr>
<tr><td>get the leader's name</td><td>@.members[0].name</td><td>Miles Davis</td></tr>
<tr><td>get the rhythm section</td><td>@.members[2:5]</td><td>(last three obs)</td></tr>
<table>