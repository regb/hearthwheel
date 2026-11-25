"""Makes every sound and the music of the game. Run as `python3 audio.py <audio dir>`.

Everything is synthesized here, in mono 16 bit WAV, which every backend plays.
"""
import array, math, os, random, sys, wave

TAU = math.tau

def save(path, samples, rate):
    peak = max(1e-6, max(abs(s) for s in samples))
    k = 30000 / peak
    with wave.open(path, "wb") as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(rate)
        w.writeframes(array.array("h", [int(s * k) for s in samples]).tobytes())

# ------------------------------------------------------------------ effects
R = 22050

def tone(seconds, f0, f1=None, shape="sine", attack=0.004, decay=6.0, vibrato=0.0, noise=0.0, seed=1):
    rnd = random.Random(seed)
    n = int(seconds * R)
    out, phase = [], 0.0
    f1 = f0 if f1 is None else f1
    low = 0.0
    for i in range(n):
        t = i / R
        u = i / n
        f = f0 * (f1 / f0) ** u
        if vibrato: f *= 1 + vibrato * math.sin(TAU * 7 * t)
        phase += f / R
        p = phase % 1.0
        if shape == "sine": s = math.sin(TAU * p)
        elif shape == "tri": s = 4 * abs(p - 0.5) - 1
        elif shape == "saw": s = 2 * p - 1
        else: s = 1.0 if p < 0.5 else -1.0
        if noise:
            low += (rnd.uniform(-1, 1) - low) * 0.5
            s = s * (1 - noise) + low * noise * 1.6
        env = min(1.0, t / attack) * math.exp(-decay * t) * min(1.0, (n - i) / (0.01 * R))
        out.append(s * env)
    return out

def hiss(seconds, cutoff0, cutoff1, attack=0.002, decay=8.0, seed=2):
    rnd = random.Random(seed)
    n = int(seconds * R)
    out, y1, y2 = [], 0.0, 0.0
    for i in range(n):
        u = i / n
        c = cutoff0 * (cutoff1 / cutoff0) ** u
        k = 1 - math.exp(-TAU * c / R)
        y1 += (rnd.uniform(-1, 1) - y1) * k
        y2 += (y1 - y2) * k
        t = i / R
        out.append(y2 * min(1.0, t / attack) * math.exp(-decay * t) * min(1.0, (n - i) / (0.01 * R)))
    return out

def mix(*parts):
    """Each part: (samples, gain, start in seconds)."""
    n = max(int(s * R) + len(p) for p, _, s in parts)
    out = [0.0] * n
    for p, g, s in parts:
        o = int(s * R)
        for i, v in enumerate(p): out[o + i] += v * g
    return out

def bell(f, seconds=0.8, decay=5.0):
    return mix((tone(seconds, f, decay=decay), 1, 0), (tone(seconds, f * 2.01, decay=decay * 1.5), 0.4, 0), (tone(seconds, f * 3.02, decay=decay * 2.2), 0.22, 0))

def effects(d):
    fx = {
        "sling": mix((hiss(0.09, 5000, 1200, decay=34), 1, 0), (tone(0.07, 520, 190, "tri", decay=30), 0.8, 0)),
        "lob": mix((tone(0.22, 210, 70, "sine", decay=13), 1, 0), (hiss(0.16, 1800, 300, decay=20), 1.4, 0)),
        "bolt": mix((tone(0.16, 320, 110, "saw", decay=24), 0.5, 0), (hiss(0.12, 3600, 700, decay=26), 1, 0), (tone(0.25, 95, 70, decay=14), 0.8, 0)),
        "zap": mix((tone(0.2, 1700, 340, "square", decay=17, noise=0.6, seed=3), 0.7, 0), (hiss(0.2, 9000, 2000, decay=18), 0.8, 0)),
        "beam": mix((tone(0.4, 180, 190, "saw", attack=0.06, decay=3.2, vibrato=0.02), 0.5, 0), (tone(0.4, 722, 735, "sine", attack=0.06, decay=3.4), 0.5, 0)),
        "spray": hiss(0.3, 6000, 3500, attack=0.05, decay=7),
        "boom": mix((hiss(0.6, 1500, 90, decay=7.5, seed=8), 1.6, 0), (tone(0.5, 110, 36, decay=8), 1.1, 0)),
        "sting": mix((tone(0.1, 900, 320, "tri", decay=26), 1, 0), (hiss(0.06, 7000, 2500, decay=40), 0.5, 0)),
        "spark": mix((tone(0.11, 1250, 620, "sine", decay=26), 1, 0), (tone(0.06, 2600, 1800, decay=40), 0.4, 0)),
        "chomp": mix((hiss(0.14, 2200, 420, decay=22, seed=5), 1.5, 0), (tone(0.1, 160, 90, "square", decay=26), 0.35, 0)),
        "built": mix((tone(0.06, 900, 700, "tri", decay=36), 0.8, 0), (hiss(0.04, 5000, 2000, decay=60), 0.8, 0), (bell(1180, 0.3, 13), 0.4, 0.05)),
        "place": mix((tone(0.06, 360, 240, "tri", decay=34), 1, 0), (hiss(0.04, 3000, 900, decay=50), 0.8, 0)),
        "break": mix((hiss(0.28, 3500, 400, decay=13, seed=9), 1.4, 0), (tone(0.2, 190, 70, "tri", decay=15), 0.8, 0)),
        "season": mix((bell(523.25, 1.8, 2.4), 0.7, 0), (bell(659.25, 1.7, 2.4), 0.6, 0.16), (bell(783.99, 1.7, 2.4), 0.6, 0.32), (bell(1046.5, 1.9, 2.2), 0.6, 0.5)),
        "wave": mix((tone(0.9, 110, 104, "saw", attack=0.12, decay=2.2, vibrato=0.012), 0.6, 0), (tone(0.9, 165, 156, "saw", attack=0.12, decay=2.4), 0.35, 0), (hiss(0.5, 500, 200, attack=0.1, decay=4), 0.6, 0)),
        "alarm": mix(*[(tone(0.26, 392, 370, "square", attack=0.02, decay=4), 0.5, 0.3 * k) for k in range(3)], *[(tone(0.26, 587, 554, "tri", attack=0.02, decay=4), 0.5, 0.3 * k) for k in range(3)]),
        "learn": mix((bell(659.25, 0.9, 4), 0.7, 0), (bell(987.77, 0.9, 4), 0.6, 0.1), (bell(1318.5, 1.1, 3.6), 0.6, 0.2)),
        "click": mix((tone(0.04, 760, 560, "tri", decay=60), 1, 0), (hiss(0.02, 6000, 3000, decay=90), 0.4, 0)),
        "deny": mix((tone(0.09, 190, 170, "square", decay=18), 0.5, 0), (tone(0.12, 150, 130, "square", decay=16), 0.5, 0.09)),
        "win": mix(*[(bell(f, 1.8, 2.2), 0.6, 0.14 * k) for k, f in enumerate([523.25, 659.25, 783.99, 1046.5, 1318.5])]),
        "lose": mix(*[(tone(1.2, f, f * 0.97, "tri", attack=0.03, decay=2.4), 0.5, 0.28 * k) for k, f in enumerate([329.6, 261.6, 220, 164.8])]),
        "ice": mix((hiss(0.4, 9000, 3000, decay=12, seed=4), 0.9, 0), *[(tone(0.2, f, f * 0.9, decay=20), 0.4, 0.03 * k) for k, f in enumerate([2400, 3100, 1800, 2700])]),
        "rail": mix((tone(0.5, 1400, 120, "saw", decay=9, noise=0.4, seed=12), 0.6, 0), (tone(0.5, 82, 46, decay=7), 1.2, 0), (hiss(0.4, 8000, 900, decay=11), 0.9, 0)),
        "rocket": mix((hiss(0.5, 1200, 4200, attack=0.03, decay=7, seed=14), 1.4, 0), (tone(0.3, 180, 300, "saw", decay=9), 0.25, 0)),
        "flame": hiss(0.3, 900, 1700, attack=0.05, decay=6, seed=15),
        "dig": mix((hiss(0.07, 3200, 1400, decay=42, seed=16), 1.1, 0), (tone(0.06, 620, 420, "tri", decay=40), 0.7, 0)),
        "deed": mix(*[(bell(f, 1.3, 3), 0.6, 0.11 * k) for k, f in enumerate([783.99, 987.77, 1174.66, 1567.98])]),
        "horn": mix((tone(1.1, 220, 222, "saw", attack=0.08, decay=1.6, vibrato=0.008), 0.5, 0), (tone(1.1, 330, 333, "saw", attack=0.1, decay=1.8), 0.35, 0.05)),
        "death": mix((hiss(0.16, 2600, 500, decay=20, seed=6), 1, 0), (tone(0.16, 420, 120, "tri", decay=18), 0.7, 0)),
    }
    for name, samples in fx.items(): save(os.path.join(d, name + ".wav"), samples, R)
    return len(fx)

# -------------------------------------------------------------------- music
M = 16000

def note(kind, f, seconds):
    n = int(seconds * M)
    out = [0.0] * n
    if kind == "pluck":
        partials = [(1, 1, 3.2), (2, 0.42, 5), (3, 0.2, 7.5), (4.02, 0.1, 10)]
    elif kind == "box":
        partials = [(1, 1, 2.4), (3.01, 0.32, 6), (5.4, 0.16, 11)]
    elif kind == "bass":
        partials = [(1, 1, 1.5), (2, 0.3, 2.5), (3, 0.1, 4)]
    else:  # pad
        partials = [(1, 1, 0), (1.004, 0.8, 0), (2, 0.25, 0), (3, 0.08, 0)]
    for k, g, d in partials:
        if f * k > M * 0.45: continue
        w = TAU * f * k / M
        if kind == "pad":
            for i in range(n):
                u = i / n
                out[i] += math.sin(w * i) * g * math.sin(math.pi * min(1, u)) ** 0.7
        else:
            dm = math.exp(-d / M)
            e = g
            for i in range(n):
                out[i] += math.sin(w * i) * e
                e *= dm
    fade = int(0.02 * M)
    for i in range(min(40, n)): out[i] *= i / 40
    for i in range(min(fade, n)): out[n - 1 - i] *= i / fade
    return out

def compose(seed, bpm, scale, root, chords, kind, density, lead_gain, pad_gain, bass_gain, octave):
    rnd = random.Random(seed)
    beat = 60 / bpm
    bars = len(chords)
    total = int(bars * 4 * beat * M)
    out = [0.0] * total
    cache = {}
    def hz(step):
        o, i = divmod(step, len(scale))
        return root * 2 ** (o + scale[i] / 12)
    def put(kind, f, start, seconds, gain):
        key = (kind, round(f, 2), round(seconds, 3))
        if key not in cache: cache[key] = note(kind, f, seconds)
        o = int(start * M)
        # Tails wrap around, so that the loop has no seam.
        for i, v in enumerate(cache[key]): out[(o + i) % total] += v * gain
    motif = [rnd.choice([0, 1, 2, 3, 4, 5]) for _ in range(8)]
    last = 2
    for b, chord in enumerate(chords):
        t0 = b * 4 * beat
        for k in (0, 2, 4): put("pad", hz(chord + k) * 0.5, t0, 4 * beat * 1.15, pad_gain)
        put("bass", hz(chord) * 0.25, t0, 2.4 * beat, bass_gain)
        put("bass", hz(chord + (4 if b % 2 else 2)) * 0.25, t0 + 2 * beat, 2 * beat, bass_gain * 0.7)
        for e in range(8):
            strong = e % 2 == 0
            if rnd.random() < density * (1 if strong else 0.55):
                if b % 4 < 2 or rnd.random() < 0.5: step = motif[(e + (b % 2) * 3) % 8] + chord
                else: step = last + rnd.choice([-2, -1, 1, 2])
                step = max(0, min(11, step))
                last = step
                put(kind, hz(step) * octave, t0 + e * beat / 2 + rnd.uniform(0, 0.012), beat * 2.6, lead_gain * rnd.uniform(0.6, 1))
    # A short echo.
    d = int(beat * 0.75 * M)
    wet = [0.0] * total
    for i in range(total): wet[i] = out[i] + 0.28 * out[(i - d) % total] + 0.1 * out[(i - 2 * d) % total]
    return wet

def ambience():
    """Loops of what the air of each season sounds like."""
    def bed(seconds, seed, cutoff, swell, gain):
        rnd = random.Random(seed)
        n = int(seconds * M)
        out, y1, y2 = [0.0] * n, 0.0, 0.0
        for i in range(n):
            t = i / n
            c = cutoff * (1 + swell * math.sin(TAU * t) + swell * 0.5 * math.sin(TAU * 3 * t + 1))
            k = 1 - math.exp(-TAU * max(40, c) / M)
            y1 += (rnd.uniform(-1, 1) - y1) * k
            y2 += (y1 - y2) * k
            out[i] = y2 * gain * (1 + swell * 0.6 * math.sin(TAU * t))
        # The ends meet.
        f = int(0.4 * M)
        for i in range(f):
            u = i / f
            out[i] = out[i] * u + out[n - f + i] * (1 - u)
        return out[:n - f]
    def chirps(out, seed, count, f0, f1, length, gain, repeat):
        rnd = random.Random(seed)
        for _ in range(count):
            o = rnd.randrange(len(out))
            f = rnd.uniform(f0, f1)
            for rep in range(repeat):
                start = o + int(rep * length * 1.6 * M)
                m = int(length * M)
                for i in range(m):
                    u = i / m
                    out[(start + i) % len(out)] += math.sin(TAU * f * (1 + 0.15 * math.sin(math.pi * u)) * i / M) * gain * math.sin(math.pi * u) ** 2
        return out
    spring = chirps(bed(9, 1, 2600, 0.2, 1.2), 2, 16, 2200, 3600, 0.07, 0.5, 3)
    summer = chirps(bed(8, 3, 500, 0.2, 0.7), 4, 26, 3900, 4300, 0.03, 0.22, 8)
    autumn = bed(10, 5, 700, 0.6, 2.0)
    winter = bed(10, 7, 420, 0.75, 2.4)
    return {"amb_spring": spring, "amb_summer": summer, "amb_autumn": autumn, "amb_winter": winter}

def music(d):
    major, minor, dorian = [0, 2, 4, 7, 9], [0, 2, 3, 5, 7, 8, 10], [0, 2, 3, 5, 7, 9, 10]
    tracks = {
        "music_spring": compose(7, 92, [0, 2, 4, 5, 7, 9, 11], 261.63, [0, 4, 5, 3, 0, 4, 3, 4], "pluck", 0.72, 0.5, 0.10, 0.34, 2),
        "music_summer": compose(21, 78, [0, 2, 4, 5, 7, 9, 10], 233.08, [0, 6, 3, 0, 0, 6, 3, 4], "pluck", 0.58, 0.5, 0.14, 0.4, 1),
        "music_autumn": compose(33, 84, dorian, 220.0, [0, 3, 6, 0, 0, 2, 3, 4], "pluck", 0.66, 0.5, 0.13, 0.4, 2),
        "music_winter": compose(45, 66, minor, 246.94, [0, 5, 2, 6, 0, 3, 5, 4], "box", 0.5, 0.5, 0.14, 0.34, 2),
    }
    tracks["music_battle"] = compose(57, 118, minor, 196.0, [0, 0, 5, 6, 0, 0, 3, 4], "pluck", 0.82, 0.5, 0.15, 0.62, 2)
    tracks.update(ambience())
    for name, samples in tracks.items(): save(os.path.join(d, name + ".wav"), samples, M)
    return len(tracks)

if __name__ == "__main__":
    os.makedirs(sys.argv[1], exist_ok=True)
    print(effects(sys.argv[1]), "effects,", music(sys.argv[1]), "pieces of music")
