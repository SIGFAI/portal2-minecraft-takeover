// Minecraft Takeover: the test chamber turns into a block world. Blocks are the Portal 2 cube model
// re-skinned with generated pixel textures (blocktex.py), creepers are built from blocks, TNT really blows.

::McModel <- "models/props/metal_box.mdl"
// skins: 0 stone, 1 grass, 2 dirt, 3 planks, 4 tnt, 5 creeper face
::Mc <- {
	origin = Vector(0, 0, 0),     // player feet when the mod starts
	floor = 0.0,
	dir = Vector(0.7071, 0.7071, 0),   // the stage looks along this (yaw 45)
	side = Vector(-0.7071, 0.7071, 0), // to the left of the view
	yaw = 45.0,
	blocks = [],                  // frozen world blocks { e, pos, free }
	creepers = [],
	tnts = [],
	achieved = false,
	started = false,
}

// Block size: the cube model is 36 units wide with beveled edges, scaled 1.5 so neighbours 48 apart overlap. Grid: a = forward, b = left (in blocks), h = height (0 = on the field).
::McU <- 48.0
::McS <- 1.65
::McAt <- function(a, b, h) {
	local m = ::Mc
	return m.origin + m.dir * (a * ::McU) + m.side * (b * ::McU) + Vector(0, 0, (m.floor - m.origin.z) + 4.0 + ::McU * (h + 1))
}

// A block: skin, scale, tint; frozen = stays in place (world), otherwise a normal physics prop.
::McBlock <- function(pos, skin, scale = 1.0, tint = null, frozen = true, life = 0.0, fn = null) {
	SigfProp(::McModel, pos, life, function(e):(skin, scale, tint, frozen, pos, fn) {
		e.SetAngles(0, ::Mc.yaw, 0)
		EntFireByHandle(e, "Skin", skin.tostring(), 0.0, null, null)
		SigfScale(e, scale * ::McS)
		if (tint != null) SigfColor(e, tint)
		if (frozen) EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
		if (fn != null) fn(e)
	})
}

// ---------- the world: a grass field over the chamber, a hill, a tree and a stone wall with TNT ----------
::McWorldPlan <- function() {
	local plan = []
	for (local a = 2; a <= 7; a++) for (local b = -5; b <= 5; b++) plan.append([a, b, -1, RandomInt(0, 5) == 0 ? 2 : 1, null])
	// hill: dirt base 2x3, grass on top
	for (local a = 4; a <= 5; a++) for (local b = 2; b <= 4; b++) plan.append([a, b, 0, 2, null])
	plan.append([4, 3, 1, 1, null])
	plan.append([4, 2, 1, 1, null])
	// tree: trunk 3 planks, leaves
	local ta = 5
	local tb = -3
	for (local h = 0; h < 3; h++) plan.append([ta, tb, h, 3, null])
	for (local a = -1; a <= 1; a++) for (local b = -1; b <= 1; b++) plan.append([ta + a, tb + b, 3, 1, "90 210 90"])
	plan.append([ta, tb, 4, 1, "90 210 90"])
	plan.append([ta + 1, tb, 4, 1, "90 210 90"])
	plan.append([ta - 1, tb, 4, 1, "90 210 90"])
	plan.append([ta, tb + 1, 4, 1, "90 210 90"])
	plan.append([ta, tb - 1, 4, 1, "90 210 90"])
	// stone wall with TNT inside
	for (local b = -1; b <= 1; b++) {
		plan.append([7, b, 0, 0, null])
		plan.append([7, b, 1, b == 0 ? 4 : 0, null])
	}
	return plan
}

::McBuild <- function() {
	local plan = ::McWorldPlan()
	plan.sort(function(x, y) {
		if (x[0] < y[0]) return -1
		if (x[0] > y[0]) return 1
		if (x[2] < y[2]) return -1
		if (x[2] > y[2]) return 1
		return 0
	})
	for (local i = 0; i < plan.len(); i++) {
		local p = plan[i]
		SigfIn(i * 0.05, function():(p) {
			local pos = ::McAt(p[0], p[1], p[2])
			::McBlock(pos, p[3], 1.0, p[4], true, 0.0, function(e):(pos, p) { ::Mc.blocks.append({ e = e, pos = pos, free = false, regrowing = false, skin = p[3], tint = p[4] }) })
		})
	}
}

// blocks raining from the ceiling
::McRain <- function(n) {
	for (local i = 0; i < n; i++) {
		SigfIn(i * 0.25, function() {
			local pos = ::McAt(RandomFloat(3.5, 6.5), RandomFloat(-4.5, 4.5), 5.0 + RandomFloat(0, 2))
			local skin = RandomInt(0, 2)
			::McBlock(pos, skin, RandomFloat(0.35, 0.55), null, false, 25.0)
		})
	}
}


// ---------- helpers ----------
::McFlat <- function(v) { return Vector(v.x, v.y, 0) }

::McPlay <- function(name) { SigfSound("sigf/" + name + ".wav") }

// ---------- explosions ----------
// Real explosion (fireball, push, scorch) + camera shake + a short slow motion; blocks and TNT nearby react.
::McBoom <- function(pos, radius, power, slow = true) {
	SigfSpawn("env_explosion", pos, { iMagnitude = power, iRadiusOverride = radius, spawnflags = 0 }, 4.0, null, null, [["Explode", ""]])
	SigfSpawn("env_shake", pos, { amplitude = 6, duration = 1.2, frequency = 40, radius = 900, spawnflags = 5 }, 4.0, null, null, [["StartShake", ""]])
	::McPlay("mc_boom")
	if (slow) {
		SigfCmd("host_timescale 0.3")
		SigfIn(0.35, function() { SigfCmd("host_timescale 1") })
	}
	// frozen world blocks in the blast come loose and fly
	foreach (b in ::Mc.blocks) {
		if (b.e == null || !b.e.IsValid() || b.free) continue
		local d = b.e.GetOrigin() - pos
		if (d.Length() < radius * 0.62) {
			b.free = true
			EntFireByHandle(b.e, "EnableMotion", "", 0.0, null, null)
			local dir = d + ::Mc.dir * 90 + Vector(0, 0, 70)
			if (dir.Length() > 1.0) dir.Norm()
			local e = b.e
			SigfIn(0.1, function():(e, dir, power) { if (e.IsValid()) SigfPush(e, dir * (power * 5.0)) })
			EntFireByHandle(b.e, "Kill", "", 5.0, null, null)
		}
	}
	// creepers and TNT in range
	foreach (c in ::Mc.creepers) {
		if (c.state == "dead") continue
		local d = (c.pos - pos).Length()
		if (d < radius) ::McHurt(c, 1 + (radius - d) / radius * 4.0, pos)
	}
	foreach (t in ::Mc.tnts) {
		if (t.state == "armed" && t.e.IsValid() && (t.e.GetOrigin() - pos).Length() < radius) t.left = 0.25
	}
}

// ---------- TNT ----------
::McTnt <- function(pos, fuse = 2.4) {
	::McBlock(pos, 4, 0.9, null, false, 20.0, function(e):(fuse) {
		::Mc.tnts.append({ e = e, left = fuse, state = "armed", blink = 0.0, skin = 4 })
		::McPlay("mc_fuse")
	})
}

// ---------- creepers ----------
// A creeper is 7 block pieces moved as one body: head with the face, two body blocks, four feet.
::McCreeperParts <- function() {
	// [forward, left, up, scale, skin, tint]
	return [
		[ 11,  10, 7, 0.40, 1, "175 255 175"],
		[ 11, -10, 7, 0.40, 1, "175 255 175"],
		[-11,  10, 7, 0.40, 1, "175 255 175"],
		[-11, -10, 7, 0.40, 1, "175 255 175"],
		[  0,   0, 32, 0.85, 1, "175 255 175"],
		[  0,   0, 62, 0.85, 1, "175 255 175"],
		[  0,   0, 98, 0.95, 5, "255 255 255"],
	]
}

::McFaceYaw <- 0.0
::McCreeperK <- 1.25

::McCreeper <- function(pos, hp = 3.0) {
	local c = { pos = pos, yaw = 0.0, hp = hp, state = "walk", fuse = 0.0, phase = 0.0, hurt = 0.0, knock = Vector(0, 0, 0), parts = [], flash = false, size = ::McCreeperK }
	local n = 0
	foreach (d in ::McCreeperParts()) {
		local part = { d = d, e = null }
		c.parts.append(part)
		SigfIn(n * 0.1, function():(part, d, pos) {
			SigfProp(::McModel, pos + Vector(0, 0, d[2]), 0, function(e):(part, d) {
				EntFireByHandle(e, "Skin", d[4].tostring(), 0.0, null, null)
				SigfScale(e, d[3] * ::McCreeperK)
				SigfColor(e, d[5])
				EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
				part.e = e
			})
		})
		n++
	}
	::Mc.creepers.append(c)
	return c
}

// Moves every piece to the creeper's current position, facing and walk animation.
::McCreeperPose <- function(c) {
	local r = c.yaw / 57.29578
	local fwd = Vector(cos(r), sin(r), 0)
	local left = Vector(-sin(r), cos(r), 0)
	local k = c.size
	local i = 0
	foreach (p in c.parts) {
		i++
		if (p.e == null || !p.e.IsValid()) continue
		local d = p.d
		local lift = 0.0
		local push = 0.0
		if (i <= 4 && c.state == "walk") {
			local s = sin(c.phase + ((i == 1 || i == 4) ? 0.0 : 3.1416))
			lift = s > 0 ? s * 5.0 : 0.0
			push = s * 5.0
		}
		local sway = (i >= 5 && c.state == "walk") ? sin(c.phase * 0.5) * 1.5 : 0.0
		local pos = c.pos + fwd * ((d[0] + push) * k) + left * ((d[1] + sway) * k) + Vector(0, 0, (d[2] + lift) * k)
		p.e.SetOrigin(pos)
		p.e.SetAngles(0, c.yaw + (i == 7 ? ::McFaceYaw : 0.0), 0)
	}
}

::McCreeperTint <- function(c, rgb) {
	local i = 0
	foreach (p in c.parts) {
		i++
		if (p.e == null || !p.e.IsValid()) continue
		if (i == 7 && rgb == "175 255 175") SigfColor(p.e, "255 255 255")
		else SigfColor(p.e, rgb)
	}
}

::McCreeperScale <- function(c, k) {
	c.size = k
	foreach (p in c.parts) { if (p.e != null && p.e.IsValid()) SigfScale(p.e, p.d[3] * k) }
}

// A hit: red flash, knock-back, hurt sound; at 0 health the creeper bursts into flying blocks.
::McHurt <- function(c, dmg, from) {
	if (c.state == "dead") return
	c.hp -= dmg
	c.hurt = 0.35
	local away = ::McFlat(c.pos - from)
	if (away.Length() < 1.0) away = Vector(1, 0, 0)
	away.Norm()
	c.knock = away * 220.0
	::McPlay("mc_hurt")
	if (c.hp <= 0) ::McDie(c)
	else ::McCreeperTint(c, "255 70 70")
}

::McDie <- function(c) {
	c.state = "dead"
	::McPlay("mc_pop")
	SigfSpawn("env_explosion", c.pos + Vector(0, 0, 40), { iMagnitude = 30, iRadiusOverride = 90, spawnflags = 17 }, 3.0, null, null, [["Explode", ""]])
	foreach (p in c.parts) {
		if (p.e == null || !p.e.IsValid()) continue
		local e = p.e
		SigfColor(e, "255 255 255")
		EntFireByHandle(e, "EnableMotion", "", 0.0, null, null)
		local v = Vector(RandomFloat(-1, 1), RandomFloat(-1, 1), RandomFloat(0.6, 1.6))
		SigfIn(0.1, function():(e, v) { if (e.IsValid()) SigfPush(e, v * 260.0) })
		EntFireByHandle(e, "Kill", "", 3.5, null, null)
	}
	SigfText("Creeper was blown up", 0.03, 0.80, 4, "255 255 255", 1)
	if (!::Mc.achieved) {
		::Mc.achieved = true
		SigfIn(1.2, function() {
			SigfText("Advancement made: Monster Hunter", 0.03, 0.84, 4, "255 255 85", 1)
			::McPlay("mc_pling")
		})
	}
}

::McCreeperTick <- function(c, dt, host) {
	if (c.state == "dead") return
	local hp = host.GetOrigin()
	local to = ::McFlat(hp) - ::McFlat(c.pos)
	local dist = to.Length()
	if (dist > 1.0) c.yaw = atan2(to.y, to.x) * 57.29578
	if (c.hurt > 0) {
		c.hurt -= dt
		if (c.hurt <= 0) ::McCreeperTint(c, "175 255 175")
	}
	if (c.knock.Length() > 5.0) {
		c.pos = c.pos + c.knock * dt
		c.knock = c.knock * 0.8
	}
	if (c.state == "walk") {
		if (dist > 150.0) {
			local dir = to * (1.0 / dist)
			c.pos = c.pos + dir * (62.0 * dt)
			c.phase += dt * 9.0
		} else {
			c.state = "fuse"
			c.fuse = 1.5
			::McPlay("mc_hiss")
		}
	} else if (c.state == "fuse") {
		c.fuse -= dt
		local k = ::McCreeperK * (1.0 + (1.5 - c.fuse) * 0.22)
		::McCreeperScale(c, k)
		local on = (floor(c.fuse * 8.0).tointeger() % 2) == 0
		if (on != c.flash) { c.flash = on; ::McCreeperTint(c, on ? "255 255 255" : "175 255 175") }
		if (c.fuse <= 0) {
			c.state = "dead"
			foreach (p in c.parts) { if (p.e != null && p.e.IsValid()) p.e.Destroy() }
			::McBoom(c.pos + Vector(0, 0, 40), 210, 160)
		}
	}
	::McCreeperPose(c)
}

// Thrown cubes and blocks hurt creepers: a prop moving fast near one counts as a hit.
::McPropSpeed <- {}
::McHitScan <- function(dt) {
	local now = Time()
	foreach (c in ::Mc.creepers) {
		if (c.state == "dead") continue
		foreach (e in SigfNear(c.pos + Vector(0, 0, 45), 90)) {
			local cls = e.GetClassname()
			if (cls != "prop_weighted_cube" && cls != "prop_physics") continue
			local id = e.entindex()
			local o = e.GetOrigin()
			if (id in ::McPropSpeed) {
				local s = ::McPropSpeed[id]
				local el = now - s.t
				if (el < 0.05) el = 0.05
				local v = (o - s.pos).Length() / el
				if (v > 260.0 && now > s.cool) {
					s.cool = now + 0.6
					::McHurt(c, 1.0, o)
				}
			}
		}
	}
	// remember where props near creepers were, to estimate their speed next time
	foreach (c in ::Mc.creepers) {
		if (c.state == "dead") continue
		foreach (e in SigfNear(c.pos + Vector(0, 0, 45), 160)) {
			local cls = e.GetClassname()
			if (cls != "prop_weighted_cube" && cls != "prop_physics") continue
			local id = e.entindex()
			local old = id in ::McPropSpeed ? ::McPropSpeed[id].cool : 0.0
			::McPropSpeed[id] <- { pos = e.GetOrigin(), t = now, cool = old }
		}
	}
}

::McTntTick <- function(dt) {
	local keep = []
	foreach (t in ::Mc.tnts) {
		if (t.state != "armed" || !t.e.IsValid()) continue
		t.left -= dt
		t.blink += dt
		if (t.blink > 0.2) {
			t.blink = 0.0
			t.skin = t.skin == 4 ? 3 : 4
			EntFireByHandle(t.e, "Skin", t.skin.tostring(), 0.0, null, null)
		}
		if (t.left <= 0) {
			t.state = "done"
			local pos = t.e.GetOrigin()
			t.e.Destroy()
			::McBoom(pos, 240, 190)
			SigfText("<TNT> ssssSSS", 0.03, 0.76, 2, "255 255 255", 1)
		} else keep.append(t)
	}
	::Mc.tnts = keep
}

::McTick <- function() {
	local host = SigfHost()
	if (host == null) return
	local dt = 0.1
	foreach (c in ::Mc.creepers) ::McCreeperTick(c, dt, host)
	::McHitScan(dt)
	::McTntTick(dt)
}

// ---------- the player throws a cube at the nearest creeper ----------
::McNearestCreeper <- function(from) {
	local best = null
	local bd = 99999.0
	foreach (c in ::Mc.creepers) {
		if (c.state == "dead") continue
		local d = (c.pos - from).Length()
		if (d < bd) { bd = d; best = c }
	}
	return best
}

::McThrow <- function(skin = -1) {
	local host = SigfHost()
	local eye = host.EyePosition()
	local c = ::McNearestCreeper(host.GetOrigin())
	if (c == null) return
	local aim = c.pos + Vector(0, 0, 45)
	local d = aim - eye
	local dist = d.Length()
	d.Norm()
	local start = eye + d * 120 + ::Mc.side * RandomFloat(-18, 18) + Vector(0, 0, -22)
	local fn = function(e):(d, dist) {
		SigfPush(e, Vector(d.x, d.y, d.z + 0.1 + dist / 5000.0) * 950.0)
	}
	if (skin < 0) SigfCube(start, 0, 3.0, fn)
	else ::McBlock(start, skin, 0.8, null, false, 3.0, fn)
}

// ---------- the world grows back ----------
::McRegrow <- function(maxn) {
	local n = 0
	foreach (b in ::Mc.blocks) {
		if (n >= maxn) break
		if (b.e != null && b.e.IsValid() && !b.free) continue
		if (b.regrowing) continue
		b.regrowing = true
		n++
		local skin = b.skin
		local tint = b.tint
		local pos = b.pos
		local entry = b
		::McBlock(pos, skin, 1.0, tint, true, 0.0, function(e):(entry) { entry.e = e; entry.free = false; entry.regrowing = false })
	}
	if (n > 0) ::McPlay("mc_place")
}

// ---------- waves ----------
::McSpawnCreeper <- function(b, a = 7.3) {
	::McCreeper(::McAt(a, b, -1) + Vector(0, 0, 24))
}

::McAlive <- function() {
	local n = 0
	foreach (c in ::Mc.creepers) { if (c.state != "dead") n++ }
	return n
}

// ---------- start ----------
// McStart builds the world; outside the demo it also runs waves of creepers and the player defends with cubes and TNT.
::McStart <- function(auto) {
	SigfAutoCam(false)
	local p = SigfHost().GetOrigin()
	::Mc.origin = p
	::Mc.floor = p.z - 8.0
	::Mc.started = true
	SigfLook(9, ::Mc.yaw)
	SigfCaption("MINECRAFT TAKEOVER", 4)
	SigfText("Steve joined the game", 0.03, 0.80, 5, "255 255 85", 1)
	::McPlay("mc_pling")
	::McRain(6)
	::McBuild()
	SigfEvery(0.1, ::McTick)
	SigfEvery(2.5, function() { ::McRegrow(12) })
	if (auto) ::McAuto()
}

// Endless waves: creepers walk in, the player throws cubes at them, a TNT block drops now and then.
::McAuto <- function() {
	SigfAfter(5.0, function() { ::McSpawnCreeper(0.5) })
	SigfEvery(9.0, function() {
		if (::McAlive() < 2) ::McSpawnCreeper(RandomFloat(-2.5, 2.5))
	})
	SigfEvery(1.3, function() {
		local c = ::McNearestCreeper(SigfHost().GetOrigin())
		if (c != null && c.state == "walk" && (c.pos - SigfHost().GetOrigin()).Length() < 520) ::McThrow()
	})
	SigfEvery(22.0, function() {
		local c = ::McNearestCreeper(SigfHost().GetOrigin())
		if (c != null && c.state == "walk") ::McTnt(c.pos + ::Mc.dir * -90 + Vector(0, 0, 20))
	})
}

if (!::SigfDemoMode) SigfAfter(0.5, function() { ::McStart(true) })
