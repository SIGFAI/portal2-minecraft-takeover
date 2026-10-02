// Demo: the chamber turns into Minecraft, creepers walk in, the player fights back with cubes and TNT.

// 0 s: the block world builds itself
SigfDemo(0.3, function() { ::McStart(false) })

// first creeper, hurt by thrown cubes
SigfDemo(7.0, function() {
	SigfCaption("A CREEPER APPEARS", 3)
	::McSpawnCreeper(0.5)
})
SigfDemo(8.9, function() { SigfText("Throw cubes to hurt it", 0.03, 0.76, 3, "255 255 255", 1); ::McThrow() })
SigfDemo(9.8, function() { ::McThrow() })
SigfDemo(10.7, function() { ::McThrow() })

// TNT kills two creepers at once
SigfDemo(15.0, function() {
	SigfCaption("TNT BLOWS THEM UP", 3)
	::McSpawnCreeper(-1.8)
	::McSpawnCreeper(1.8)
})
SigfDemo(16.4, function() { ::McTnt(::McAt(4.6, 0.0, 0)) })

// chain reaction of TNT with creepers walking into it
SigfDemo(24.0, function() {
	SigfCaption("CHAIN REACTION", 3)
	::McSpawnCreeper(-1.0)
	::McSpawnCreeper(1.0)
})
SigfDemo(25.0, function() {
	::McTnt(::McAt(5.4, -0.7, 0), 7.0)
	::McTnt(::McAt(5.4, 0.7, 0), 7.0)
})
SigfDemo(25.8, function() {
	::McTnt(::McAt(4.4, 0.0, 0), 2.6)
})

// the creepers get through: one blows up next to the player
SigfDemo(34.0, function() {
	SigfCaption("CREEPERS EVERYWHERE", 3)
	::McSpawnCreeper(-2.4)
	::McSpawnCreeper(0.0)
	::McSpawnCreeper(2.4)
})
SigfDemo(38.0, function() { ::McThrow() })
SigfDemo(39.0, function() { ::McThrow() })

// the world grows back, the camera looks around
SigfDemo(46.0, function() {
	SigfCaption("THE WORLD GROWS BACK", 3)
	SigfLook(10, 20)
})
SigfDemo(50.0, function() { SigfLook(10, 70) })
SigfDemo(54.0, function() { SigfLook(12, 45) })

// last stand
SigfDemo(56.0, function() {
	SigfCaption("ONE LAST WAVE", 3)
	::McSpawnCreeper(-1.2)
	::McSpawnCreeper(1.2)
	::McSpawnCreeper(0.0, 8.5)
})
SigfDemo(58.0, function() { ::McThrow() })
SigfDemo(59.0, function() { ::McThrow() })
SigfDemo(60.0, function() { ::McThrow() })
SigfDemo(62.0, function() { ::McTnt(::McAt(4.4, 0.0, 0), 2.4) })
SigfDemo(68.0, function() {
	SigfCaption("MINECRAFT TAKEOVER", 4)
	::McAuto()
})
