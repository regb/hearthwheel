import java.awt.*;
import java.awt.geom.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.*;
import java.util.List;
import javax.imageio.ImageIO;

/** Draws every picture of the game. Run as `java Art.java <sprites dir> <Sheet.scala>`. */
public class Art {
  static final int T = 64;
  static final Color INK = c(0x2e2230), CREAM = c(0xf3e3c3), WOOD = c(0xa9744a), WOOD_D = c(0x7c4f35), WOOD_L = c(0xcf9a66),
      COPPER = c(0xd07a45), COPPER_D = c(0x9a4f34), COPPER_L = c(0xf0a878), BRASS = c(0xd9ab4a), BRASS_D = c(0x9b7230), BRASS_L = c(0xf7dc8e),
      STONE = c(0x9c95a0), STONE_D = c(0x6c6677), STONE_L = c(0xc9c4cc), TEAL = c(0x4ba39a), TEAL_D = c(0x2c6d70), TEAL_L = c(0x96dccb),
      IRON = c(0x5c5868), IRON_D = c(0x3d3a49), IRON_L = c(0x8a8796), FIRE = c(0xff8a3c), FIRE_L = c(0xffd27a), GLASS = c(0xa8ece2),
      LEAF = c(0x6f9e45), LEAF_D = c(0x3f6a35), LEAF_L = c(0xa9d163), THORN = c(0x7a4a6a), THORN_D = c(0x4a2c48), BARK = c(0x6b4a3a), BARK_D = c(0x45302b), BARK_L = c(0x93694c);

  static File out;
  static final TreeSet<String> names = new TreeSet<>();
  static Graphics2D g;
  static Random r;

  static Color c(int rgb) { return new Color(rgb); }
  static Color a(Color k, int alpha) { return new Color(k.getRed(), k.getGreen(), k.getBlue(), alpha); }
  static Color mix(Color x, Color y, double t) {
    return new Color((int) (x.getRed() + (y.getRed() - x.getRed()) * t), (int) (x.getGreen() + (y.getGreen() - x.getGreen()) * t), (int) (x.getBlue() + (y.getBlue() - x.getBlue()) * t));
  }
  static Color gray(int v) { return new Color(v, v, v); }

  interface Draw { void run() throws Exception; }
  static void sprite(String name, int w, int h, Draw d) throws Exception {
    BufferedImage image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
    g = image.createGraphics();
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    r = new Random(name.hashCode());
    d.run();
    g.dispose();
    ImageIO.write(image, "png", new File(out, name + ".png"));
    names.add(name);
  }
  static void sprite(String name, int s, Draw d) throws Exception { sprite(name, s, s, d); }

  // ----------------------------------------------------------------- shapes
  static Shape rr(double x, double y, double w, double h, double rad) { return new RoundRectangle2D.Double(x, y, w, h, rad * 2, rad * 2); }
  static Shape rect(double x, double y, double w, double h) { return new Rectangle2D.Double(x, y, w, h); }
  static Shape circle(double x, double y, double rad) { return new Ellipse2D.Double(x - rad, y - rad, rad * 2, rad * 2); }
  static Shape oval(double x, double y, double rx, double ry) { return new Ellipse2D.Double(x - rx, y - ry, rx * 2, ry * 2); }
  static Shape poly(double... p) {
    Path2D path = new Path2D.Double();
    path.moveTo(p[0], p[1]);
    for (int i = 2; i < p.length; i += 2) path.lineTo(p[i], p[i + 1]);
    path.closePath();
    return path;
  }
  static Shape ring(double x, double y, double outer, double inner) {
    Area area = new Area(circle(x, y, outer));
    area.subtract(new Area(circle(x, y, inner)));
    return area;
  }
  static Shape gear(double x, double y, int teeth, double outer, double inner, double hole) {
    Path2D p = new Path2D.Double();
    for (int i = 0; i < teeth; i++) {
      double a0 = Math.PI * 2 * i / teeth, s = Math.PI * 2 / teeth;
      double[][] pts = {{a0 - s * .30, inner}, {a0 - s * .18, outer}, {a0 + s * .18, outer}, {a0 + s * .30, inner}};
      for (int k = 0; k < 4; k++) {
        double px = x + Math.cos(pts[k][0]) * pts[k][1], py = y + Math.sin(pts[k][0]) * pts[k][1];
        if (i == 0 && k == 0) p.moveTo(px, py); else p.lineTo(px, py);
      }
    }
    p.closePath();
    Area area = new Area(p);
    if (hole > 0) area.subtract(new Area(circle(x, y, hole)));
    return area;
  }
  static Shape star(double x, double y, int n, double outer, double inner, double turn) {
    Path2D p = new Path2D.Double();
    for (int i = 0; i < n * 2; i++) {
      double an = turn + Math.PI * i / n, rad = i % 2 == 0 ? outer : inner;
      if (i == 0) p.moveTo(x + Math.cos(an) * rad, y + Math.sin(an) * rad); else p.lineTo(x + Math.cos(an) * rad, y + Math.sin(an) * rad);
    }
    p.closePath();
    return p;
  }
  static Shape blob(double x, double y, double rad, double wobble, int n, Random rnd) {
    double[] rs = new double[n];
    for (int i = 0; i < n; i++) rs[i] = rad * (1 - wobble + rnd.nextDouble() * wobble * 2);
    Path2D p = new Path2D.Double();
    for (int i = 0; i <= n; i++) {
      double a0 = Math.PI * 2 * i / n, a1 = Math.PI * 2 * (i + 1) / n;
      double x0 = x + Math.cos(a0) * rs[i % n], y0 = y + Math.sin(a0) * rs[i % n];
      double x1 = x + Math.cos(a1) * rs[(i + 1) % n], y1 = y + Math.sin(a1) * rs[(i + 1) % n];
      if (i == 0) p.moveTo((x0 + x1) / 2, (y0 + y1) / 2); else p.quadTo(x0, y0, (x0 + x1) / 2, (y0 + y1) / 2);
    }
    p.closePath();
    return p;
  }
  static Shape turned(Shape s, double angle, double x, double y) { return AffineTransform.getRotateInstance(angle, x, y).createTransformedShape(s); }

  static void fill(Shape s, Paint p) { g.setPaint(p); g.fill(s); }
  static void line(Shape s, Color k, double w) { g.setPaint(k); g.setStroke(new BasicStroke((float) w, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)); g.draw(s); }
  static void seg(double x0, double y0, double x1, double y1, Color k, double w) { line(new Line2D.Double(x0, y0, x1, y1), k, w); }
  /** A solid thing: lit from the top left, with an outline. */
  static void solid(Shape s, Color light, Color base, Color dark) { solid(s, light, base, dark, 3); }
  static void solid(Shape s, Color light, Color base, Color dark, double ink) {
    Rectangle2D b = s.getBounds2D();
    fill(s, new LinearGradientPaint((float) b.getMinX(), (float) b.getMinY(), (float) b.getMaxX() + .1f, (float) b.getMaxY() + .1f, new float[]{0f, .45f, 1f}, new Color[]{light, base, dark}));
    Shape clip = g.getClip();
    g.clip(s);
    g.translate(1.5, 1.5);
    line(s, a(Color.WHITE, 70), ink + 2);
    g.translate(-1.5, -1.5);
    g.setClip(clip);
    if (ink > 0) line(s, INK, ink);
  }
  static void copper(Shape s) { solid(s, COPPER_L, COPPER, COPPER_D); }
  static void brass(Shape s) { solid(s, BRASS_L, BRASS, BRASS_D); }
  static void wood(Shape s) { solid(s, WOOD_L, WOOD, WOOD_D); }
  static void stone(Shape s) { solid(s, STONE_L, STONE, STONE_D); }
  static void iron(Shape s) { solid(s, IRON_L, IRON, IRON_D); }
  static void cream(Shape s) { solid(s, c(0xfff6e0), CREAM, c(0xc9b08c)); }
  static void teal(Shape s) { solid(s, TEAL_L, TEAL, TEAL_D); }
  static void bolt(double x, double y) { fill(circle(x, y, 2.6), INK); fill(circle(x - .5, y - .5, 1.6), BRASS_L); }
  static void bolts(double x, double y, double w, double h) { bolt(x, y); bolt(x + w, y); bolt(x, y + h); bolt(x + w, y + h); }
  static void glow(double x, double y, double rad, Color k, int alpha) {
    fill(circle(x, y, rad), new RadialGradientPaint((float) x, (float) y, (float) rad, new float[]{0f, 1f}, new Color[]{a(k, alpha), a(k, 0)}));
  }
  static void planks(double x, double y, double w, double h, int n, boolean vertical) {
    Shape clip = g.getClip();
    g.clip(rr(x, y, w, h, 7));
    for (int i = 0; i < n; i++) {
      Color k = mix(WOOD, i % 2 == 0 ? WOOD_L : WOOD_D, .25 + r.nextDouble() * .2);
      if (vertical) { fill(rect(x + w * i / n, y, w / n, h), k); seg(x + w * i / n, y, x + w * i / n, y + h, a(WOOD_D, 170), 2); }
      else { fill(rect(x, y + h * i / n, w, h / n), k); seg(x, y + h * i / n, x + w, y + h * i / n, a(WOOD_D, 170), 2); }
    }
    for (int i = 0; i < n * 3; i++) {
      double px = x + r.nextDouble() * w, py = y + r.nextDouble() * h, l = 5 + r.nextDouble() * 12;
      if (vertical) seg(px, py, px, py + l, a(WOOD_D, 80), 1.4); else seg(px, py, px + l, py, a(WOOD_D, 80), 1.4);
    }
    g.setClip(clip);
    line(rr(x, y, w, h, 7), INK, 3);
  }
  /** The footing of a machine: a cream slab with a wooden frame. */
  static void footing(int size) {
    double s = size * T;
    solid(rr(3, 3, s - 6, s - 6, 11), WOOD_L, WOOD, WOOD_D, 3.4);
    solid(rr(9, 9, s - 18, s - 18, 7), c(0xe9d8b8), c(0xcdb894), c(0xa08867), 2);
    bolts(9, 9, s - 18, s - 18);
  }
  static void stonefoot(int size) {
    double s = size * T;
    solid(rr(3, 3, s - 6, s - 6, 13), STONE_L, STONE, STONE_D, 3.4);
    for (int i = 0; i < size * size * 4; i++) {
      double x = 8 + r.nextDouble() * (s - 22), y = 8 + r.nextDouble() * (s - 22);
      line(rr(x, y, 8 + r.nextDouble() * 8, 5 + r.nextDouble() * 5, 3), a(STONE_D, 90), 1.4);
    }
    line(rr(9, 9, s - 18, s - 18, 9), a(INK, 70), 2);
  }

  // ------------------------------------------------------------------ the land
  static void land() throws Exception {
    // Neutral, to be tinted by the season.
    for (int v = 0; v < 4; v++) {
      sprite("f_meadow" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(226));
        for (int i = 0; i < 26; i++) fill(blob(r.nextInt(T), r.nextInt(T), 3 + r.nextInt(8), .3, 7, r), a(gray(r.nextBoolean() ? 242 : 208), 70));
        for (int i = 0; i < 24; i++) {
          double x = 4 + r.nextInt(56), y = 6 + r.nextInt(54);
          Color k = a(gray(r.nextInt(3) == 0 ? 255 : 176), 150);
          seg(x, y, x - 2 + r.nextInt(3), y - 4 - r.nextInt(4), k, 1.5);
          if (r.nextBoolean()) seg(x, y, x + 3, y - 3, k, 1.4);
        }
      });
      sprite("f_loam" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(222));
        for (int i = 0; i < 22; i++) fill(blob(r.nextInt(T), r.nextInt(T), 3 + r.nextInt(7), .3, 7, r), a(gray(r.nextBoolean() ? 240 : 198), 70));
        for (int i = 0; i < 14; i++) { double x = r.nextInt(T), y = r.nextInt(T); fill(oval(x, y, 1.5 + r.nextDouble() * 2, 1.2), a(gray(170), 120)); }
      });
      sprite("f_gravel" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(212));
        for (int i = 0; i < 34; i++) {
          double x = 4 + r.nextInt(56), y = 4 + r.nextInt(56), s = 2 + r.nextDouble() * 4;
          fill(blob(x + 1, y + 1.4, s, .2, 6, r), a(gray(140), 110));
          fill(blob(x, y, s, .2, 6, r), a(gray(206 + r.nextInt(46)), 200));
        }
      });
      sprite("f_sand" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(236));
        for (int i = 0; i < 5; i++) {
          double y = 6 + r.nextInt(52), x = r.nextInt(24);
          Path2D p = new Path2D.Double();
          p.moveTo(x, y); p.curveTo(x + 10, y - 4, x + 18, y + 4, x + 30 + r.nextInt(8), y);
          line(p, a(gray(206), 120), 2);
        }
        for (int i = 0; i < 30; i++) fill(circle(r.nextInt(T), r.nextInt(T), .9), a(gray(r.nextBoolean() ? 255 : 196), 160));
      });
      sprite("f_water" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(228));
        for (int i = 0; i < 9; i++) fill(oval(r.nextInt(T), r.nextInt(T), 6 + r.nextInt(12), 2 + r.nextInt(4)), a(gray(r.nextBoolean() ? 250 : 206), 70));
      });
      sprite("f_ice" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(238));
        for (int i = 0; i < 3; i++) {
          double x = r.nextInt(T), y = r.nextInt(T);
          for (int k = 0; k < 4; k++) { double nx = x + r.nextInt(28) - 14, ny = y + r.nextInt(28) - 14; seg(x, y, nx, ny, a(Color.WHITE, 190), 1.6); seg(x + 1, y + 1, nx + 1, ny + 1, a(gray(190), 90), 1.2); x = nx; y = ny; }
        }
      });
      sprite("f_bed" + v, T, () -> {
        fill(rect(0, 0, T, T), gray(220));
        for (int i = 0; i < 6; i++) {
          double x = r.nextInt(T), y = r.nextInt(T);
          for (int k = 0; k < 4; k++) { double nx = x + r.nextInt(26) - 13, ny = y + r.nextInt(26) - 13; seg(x, y, nx, ny, a(gray(156), 150), 1.7); x = nx; y = ny; }
        }
      });
    }
    // Ripples and glints that move on open water.
    for (int v = 0; v < 4; v++) {
      final int n = v;
      sprite("ripple" + v, T, () -> {
        for (int i = 0; i < 3; i++) {
          double x = 8 + r.nextInt(30), y = 10 + i * 19 + n * 2, l = 9 + r.nextInt(14);
          Path2D p = new Path2D.Double();
          p.moveTo(x, y); p.quadTo(x + l / 2, y - 4, x + l, y);
          line(p, a(Color.WHITE, 150), 2.2);
        }
      });
    }
    // A soft uneven fringe, along the top side and in the top left corner.
    for (int v = 0; v < 2; v++) {
      sprite("fringe_side" + v, T, 40, () -> {
        Path2D p = new Path2D.Double();
        p.moveTo(0, 0); p.lineTo(T, 0); p.lineTo(T, 14);
        double x = T;
        double prev = 14;
        while (x > 0) { double nx = Math.max(0, x - 9 - r.nextInt(9)); double ny = nx == 0 ? 14 : 5 + r.nextInt(25); p.curveTo(x - 4, prev, nx + 4, ny, nx, ny); x = nx; prev = ny; }
        p.closePath();
        fill(p, new GradientPaint(0, 0, gray(226), 0, 34, gray(212)));
        // Blades and grains that stick out further.
        for (int i = 0; i < 9; i++) { double bx = 4 + r.nextInt(56), by = 14 + r.nextInt(16); fill(blob(bx, by, 2 + r.nextInt(3), .3, 6, r), gray(216)); }
      });
      sprite("fringe_corner" + v, 40, 40, () -> {
        Path2D p = new Path2D.Double();
        p.moveTo(0, 0); p.lineTo(17 + r.nextInt(6), 0); p.quadTo(17, 17, 0, 17 + r.nextInt(6)); p.closePath();
        fill(p, gray(222));
        for (int i = 0; i < 3; i++) fill(blob(8 + r.nextInt(12), 8 + r.nextInt(12), 3, .3, 6, r), gray(216));
      });
      // Foam and wet edge on water.
      sprite("shore_side" + v, T, 20, () -> {
        Path2D p = new Path2D.Double();
        p.moveTo(0, 6);
        double x = 0;
        while (x < T) { double nx = Math.min(T, x + 8 + r.nextInt(7)); p.quadTo((x + nx) / 2, 2 + r.nextInt(8), nx, nx == T ? 6 : 5 + r.nextInt(4)); x = nx; }
        line(p, a(Color.WHITE, 130), 3);
        line(p, a(Color.WHITE, 60), 7);
      });
    }
    // Cliffs: boulders that overlap into ridges.
    for (int v = 0; v < 4; v++) {
      sprite("cliff" + v, 92, () -> {
        Shape base = blob(46, 49, 38, .10, 9, r);
        solid(base, gray(222), gray(172), gray(112), 3.4);
        Shape clip = g.getClip();
        g.clip(base);
        fill(poly(12, 40, 34, 12, 62, 16, 54, 38, 28, 48), a(Color.WHITE, 60));
        fill(poly(54, 38, 62, 16, 88, 34, 80, 60), a(Color.BLACK, 22));
        fill(poly(28, 48, 54, 38, 80, 60, 60, 92, 30, 86), a(Color.BLACK, 46));
        seg(28, 48, 54, 38, a(Color.WHITE, 90), 2);
        seg(54, 38, 62, 16, a(Color.WHITE, 60), 2);
        seg(54, 38, 80, 60, a(Color.BLACK, 50), 2);
        for (int i = 0; i < 4; i++) { double x = 20 + r.nextInt(52), y = 22 + r.nextInt(50); seg(x, y, x + r.nextInt(12) - 6, y + 5 + r.nextInt(8), a(Color.BLACK, 60), 1.8); }
        g.setClip(clip);
      });
    }
    // Trees from above, neutral; and their winter dress.
    for (int v = 0; v < 3; v++) {
      sprite("tree" + v, 100, () -> {
        fill(blob(50, 52, 40, .12, 11, r), a(INK, 255));
        Shape crown = blob(50, 50, 38, .12, 11, r);
        fill(crown, new RadialGradientPaint(40f, 36f, 58f, new float[]{0f, .6f, 1f}, new Color[]{gray(244), gray(192), gray(128)}));
        Shape clip = g.getClip();
        g.clip(crown);
        for (int i = 0; i < 24; i++) {
          double x = 18 + r.nextInt(64), y = 18 + r.nextInt(64);
          double lit = 1 - (x + y) / 200;
          Shape leaf = blob(x, y, 7 + r.nextInt(5), .2, 8, r);
          fill(leaf, a(gray((int) (150 + lit * 100)), 130));
          line(leaf, a(gray(110), 50), 1.4);
        }
        g.setClip(clip);
        line(crown, a(INK, 150), 2.4);
      });
      sprite("bare" + v, 100, () -> {
        for (int i = 0; i < 9; i++) {
          double an = Math.PI * 2 * i / 9 + r.nextDouble() * .4, x = 50, y = 50;
          double w = 8;
          for (int k = 0; k < 4; k++) {
            double nx = x + Math.cos(an) * (10 + r.nextInt(3)), ny = y + Math.sin(an) * (10 + r.nextInt(3));
            seg(x, y, nx, ny, INK, w + 2.4); seg(x, y, nx, ny, BARK, w); seg(x - .6, y - .6, nx - .6, ny - .6, BARK_L, w * .4);
            if (k > 0) { double sa = an + (r.nextBoolean() ? .8 : -.8); seg(nx, ny, nx + Math.cos(sa) * 9, ny + Math.sin(sa) * 9, INK, 4); seg(nx, ny, nx + Math.cos(sa) * 9, ny + Math.sin(sa) * 9, BARK, 2.4); }
            x = nx; y = ny; w *= .68; an += (r.nextDouble() - .5) * .5;
          }
        }
        for (int i = 0; i < 12; i++) fill(blob(18 + r.nextInt(64), 18 + r.nextInt(64), 4 + r.nextInt(6), .25, 8, r), a(c(0xf6fbff), 235));
      });
    }
    // Snow that lies on rocks.
    sprite("snowcap", 92, () -> {
      Shape s = blob(44, 40, 29, .14, 10, r);
      fill(AffineTransform.getTranslateInstance(2, 4).createTransformedShape(s), a(c(0x8fa8c8), 120));
      fill(s, c(0xf4faff));
      fill(blob(38, 34, 16, .2, 8, r), Color.WHITE);
    });
    // Seams.
    Object[][] ores = {{"copper", c(0xe08a50), c(0x8f4a30), c(0xffc79a)}, {"tin", c(0xb9cad6), c(0x62717f), c(0xf2fbff)}, {"coal", c(0x4a4656), c(0x211e2b), c(0x8a86a0)}, {"flint", c(0xd8c2a0), c(0x7b6a55), c(0xfff2da)}, {"iron", c(0xa8746a), c(0x5c3a3a), c(0xe0b0a0)}};
    for (Object[] o : ores) for (int v = 0; v < 3; v++) {
      sprite("ore_" + o[0] + v, T, () -> {
        int n = 5;
        for (int i = 0; i < n; i++) {
          double x = 11 + r.nextInt(42), y = 11 + r.nextInt(42), s = 4.5 + r.nextDouble() * 5;
          Shape gem = blob(x, y, s, .28, 6, r);
          fill(AffineTransform.getTranslateInstance(1.6, 2.6).createTransformedShape(gem), a(INK, 110));
          solid(gem, (Color) o[3], (Color) o[1], (Color) o[2], 1.8);
          fill(blob(x - s * .3, y - s * .35, s * .32, .2, 5, r), a((Color) o[3], 220));
        }
        for (int i = 0; i < 5; i++) fill(circle(6 + r.nextInt(52), 6 + r.nextInt(52), 1 + r.nextDouble() * 1.4), (Color) o[1]);
      });
    }
    // Small things on the ground.
    Color[] petals = {c(0xfff1b8), c(0xf7b6d0), c(0xc9b8ff), c(0xffffff)};
    for (int v = 0; v < 4; v++) { final Color k = petals[v]; sprite("flower" + v, 24, () -> {
      for (int i = 0; i < 3; i++) { double x = 6 + r.nextInt(12), y = 6 + r.nextInt(12);
        seg(x, y, x, y + 5, LEAF_D, 1.6);
        for (int p = 0; p < 5; p++) fill(circle(x + Math.cos(p * 1.2566) * 2.6, y + Math.sin(p * 1.2566) * 2.6, 1.9), k);
        fill(circle(x, y, 1.6), c(0xf2a63a)); }
    }); }
    for (int v = 0; v < 2; v++) {
      sprite("tuft" + v, 32, () -> { for (int i = 0; i < 9; i++) { double x = 9 + r.nextInt(14); Path2D p = new Path2D.Double(); p.moveTo(x, 26); p.quadTo(x + r.nextInt(5) - 2, 16, x + r.nextInt(17) - 8, 7 + r.nextInt(8)); line(p, a(gray(180 + r.nextInt(70)), 255), 2); } });
      sprite("leaves" + v, 32, () -> { Color[] ks = {c(0xd8792f), c(0xc24f2c), c(0xe8b23a), c(0x9c5a34)}; for (int i = 0; i < 8; i++) { double x = 5 + r.nextInt(22), y = 5 + r.nextInt(22); Shape l = turned(oval(x, y, 4.2, 2.1), r.nextDouble() * 6.28, x, y); fill(AffineTransform.getTranslateInstance(.8, 1.2).createTransformedShape(l), a(INK, 70)); fill(l, ks[r.nextInt(4)]); } });
      sprite("drift" + v, 40, () -> { Shape s = turned(blob(20, 20, 13, .25, 9, r), 0, 20, 20); fill(AffineTransform.getTranslateInstance(1, 2.5).createTransformedShape(s), a(c(0x9db4d0), 130)); fill(s, a(c(0xf6fbff), 240)); fill(blob(17, 16, 6, .3, 7, r), Color.WHITE); });
      sprite("pebble" + v, 24, () -> { for (int i = 0; i < 3; i++) { double x = 6 + r.nextInt(12), y = 6 + r.nextInt(12); Shape s = blob(x, y, 2 + r.nextInt(3), .2, 6, r); fill(AffineTransform.getTranslateInstance(.8, 1.4).createTransformedShape(s), a(INK, 80)); fill(s, gray(170 + r.nextInt(50))); } });
      sprite("reed" + v, 32, () -> { for (int i = 0; i < 5; i++) { double x = 8 + r.nextInt(16), y = 24 + r.nextInt(5); seg(x, y, x + r.nextInt(5) - 2, y - 15, c(0x58803c), 2); fill(oval(x, y - 16, 2, 4.5), c(0x7a5236)); } });
    }
    sprite("mushroom", 24, () -> { seg(12, 14, 12, 20, CREAM, 5); solid(new Arc2D.Double(4, 5, 16, 16, 0, 180, Arc2D.CHORD), c(0xf08a6a), c(0xd1503e), c(0x9c3430), 2); fill(circle(9, 10, 1.5), CREAM); fill(circle(14, 8.5, 1.3), CREAM); });
  }

  // ----------------------------------------------------------------- the goods
  static void goods() throws Exception {
    int s = 48;
    sprite("item_copper", s, () -> { for (double[] p : new double[][]{{16, 30, 11}, {32, 31, 10}, {24, 18, 11}}) solid(blob(p[0], p[1], p[2], .16, 7, r), COPPER_L, COPPER, COPPER_D, 2.4); });
    sprite("item_tin", s, () -> { for (double[] p : new double[][]{{17, 31, 10}, {32, 29, 11}, {23, 17, 10}}) solid(blob(p[0], p[1], p[2], .16, 7, r), c(0xf4fbff), c(0xb7c6d2), c(0x74838f), 2.4); });
    sprite("item_coal", s, () -> { for (double[] p : new double[][]{{16, 30, 11}, {33, 30, 10}, {25, 17, 11}}) solid(blob(p[0], p[1], p[2], .26, 6, r), c(0x8c88a2), c(0x47435a), c(0x232030), 2.4); });
    sprite("item_flint", s, () -> { solid(poly(8, 34, 14, 10, 28, 6, 40, 18, 40, 34, 24, 42), c(0xfff0d6), c(0xcdb693), c(0x8c765c), 2.4); fill(poly(14, 12, 27, 8, 22, 24), a(Color.WHITE, 120)); seg(22, 24, 40, 32, a(INK, 110), 2); seg(22, 24, 24, 41, a(INK, 110), 2); });
    sprite("item_resin", s, () -> { Path2D p = new Path2D.Double(); p.moveTo(24, 4); p.curveTo(40, 22, 40, 42, 24, 43); p.curveTo(8, 42, 8, 22, 24, 4); solid(p, c(0xffe08a), c(0xf2a52e), c(0xb8681c), 2.4); fill(oval(18, 28, 4, 8), a(Color.WHITE, 150)); });
    sprite("item_bronze", s, () -> { for (int i = 0; i < 2; i++) { double y = 16 + i * 12, x = 6 + i * 4; solid(poly(x, y + 12, x + 5, y, x + 27, y, x + 32, y + 12), BRASS_L, c(0xd1973c), c(0x8c5c28), 2.4); } });
    sprite("item_glass", s, () -> { solid(poly(24, 4, 42, 18, 36, 42, 12, 42, 6, 18), c(0xf2fffc), c(0x9fe0d8), c(0x4fa7a8), 2.4); fill(poly(24, 8, 12, 20, 18, 36), a(Color.WHITE, 140)); });
    sprite("item_gear", s, () -> { brass(gear(24, 24, 8, 20, 15, 6)); line(circle(24, 24, 10), a(INK, 90), 2); });
    sprite("item_ember", s, () -> { glow(24, 24, 23, FIRE, 190); solid(poly(24, 6, 38, 18, 38, 32, 24, 43, 10, 32, 10, 18), c(0xfff0b0), c(0xff7a3a), c(0xc2342a), 2.4); fill(star(24, 24, 4, 12, 4, .78), c(0xfff6cf)); });
  }

  // -------------------------------------------------------------- the buildings
  static void belt(String name, Color bed, Color bedD, Color mark, Color rail, Color railL, Color railD) throws Exception {
    for (int f = 0; f < 8; f++) {
      final double off = f / 8.0 * 32;
      sprite(name + "_s" + f, T, () -> {
        fill(rect(0, 7, T, 50), bed);
        fill(rect(0, 7, T, 9), bedD);
        for (int i = -1; i < 3; i++) { double x = i * 32 + off; Path2D p = new Path2D.Double(); p.moveTo(x, 18); p.lineTo(x + 11, 32); p.lineTo(x, 46); line(p, mark, 4.6); }
        for (int i = 0; i < 2; i++) { double y = i == 0 ? 2 : 54; fill(rect(0, y, T, 8), new GradientPaint(0, (float) y, railL, 0, (float) y + 8, railD)); seg(0, y, T, y, INK, 2.6); seg(0, y + 8, T, y + 8, INK, 2.2); }
      });
      // From the top, turning to the right.
      sprite(name + "_c" + f, T, () -> {
        Shape clip = g.getClip();
        g.clip(rect(0, 0, T, T));
        fill(ring(64, 0, 57, 7), bed);
        fill(ring(64, 0, 57, 49), bedD);
        for (int i = -1; i < 3; i++) {
          double t = (i * 32 + off) / 50.0;
          double an = Math.PI - t;
          AffineTransform before = g.getTransform();
          g.translate(64 + Math.cos(an) * 32, Math.sin(an) * 32);
          g.rotate(an - Math.PI / 2);
          Path2D p = new Path2D.Double(); p.moveTo(-5, -13); p.lineTo(5, 0); p.lineTo(-5, 13); line(p, mark, 4.6);
          g.setTransform(before);
        }
        fill(ring(64, 0, 62, 54), mix(rail, railD, .3)); line(circle(64, 0, 62), INK, 2.6); line(circle(64, 0, 54), INK, 2.2); line(circle(64, 0, 61), a(railL, 200), 1.4);
        fill(ring(64, 0, 10, 2), rail); line(circle(64, 0, 10), INK, 2.2); line(circle(64, 0, 2), INK, 2.6);
        g.setClip(clip);
      });
    }
    // The gap in a rail where a side line comes in, from the top.
    sprite(name + "_in", T, 14, () -> { fill(rect(8, 0, 48, 14), bed); fill(rect(0, 2, 9, 8), rail); fill(rect(55, 2, 9, 8), rail); seg(8, 0, 8, 11, INK, 2.6); seg(56, 0, 56, 11, INK, 2.6); });
  }

  static void haulage() throws Exception {
    belt("belt", c(0x6b5548), c(0x57443c), c(0xd8a878), COPPER, COPPER_L, COPPER_D);
    belt("covered_belt", c(0x4a4658), c(0x3a3648), c(0x9fb4d8), STONE, STONE_L, STONE_D);
    belt("swift_belt", c(0x4c5560), c(0x3e4650), c(0xf2d37a), BRASS, BRASS_L, BRASS_D);
    sprite("crossing", T, () -> { copper(rr(4, 4, 56, 56, 9)); fill(rect(5, 18, 54, 28), c(0x6b5548)); fill(rect(18, 5, 28, 54), c(0x6b5548)); for (double[] p : new double[][]{{6, 6}, {46, 6}, {6, 46}, {46, 46}}) copper(rr(p[0], p[1], 12, 12, 3)); seg(32, 10, 32, 54, c(0xd8a878), 4); seg(10, 32, 54, 32, c(0xd8a878), 4); fill(circle(32, 32, 6), CREAM); line(circle(32, 32, 6), INK, 2); });
    sprite("splitter", T, () -> { copper(rr(4, 4, 56, 56, 12)); fill(circle(32, 32, 21), c(0x57443c)); line(circle(32, 32, 21), INK, 3); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2; double x = 32 + Math.cos(an) * 22, y = 32 + Math.sin(an) * 22; fill(turned(poly(x - 4, y - 7, x + 6, y, x - 4, y + 7), an, x, y), CREAM); } brass(circle(32, 32, 8)); });
    sprite("sorter", T, () -> { teal(rr(4, 4, 56, 56, 10)); cream(circle(32, 32, 18)); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2, x = 32 + Math.cos(an) * 25, y = 32 + Math.sin(an) * 25; fill(circle(x, y, 3.5), INK); } });
    sprite("spillway", T, () -> { teal(rr(4, 4, 56, 56, 10)); fill(rr(22, 8, 20, 48, 5), c(0x35505a)); fill(rr(8, 24, 48, 16, 5), c(0x35505a)); seg(32, 48, 32, 18, CREAM, 4); fill(poly(24, 22, 32, 10, 40, 22), CREAM); fill(poly(18, 26, 10, 32, 18, 38), c(0xf2b27a)); fill(poly(46, 26, 54, 32, 46, 38), c(0xf2b27a)); });
    sprite("zipline", T, () -> { wood(rr(8, 8, 48, 48, 9)); iron(circle(32, 32, 17)); brass(gear(32, 32, 10, 15, 11, 4)); bolts(13, 13, 38, 38); });
    sprite("storehouse", T * 2, () -> { planks(5, 5, 118, 118, 7, true); solid(rr(18, 18, 92, 92, 8), COPPER_L, COPPER, COPPER_D); planks(26, 26, 76, 76, 5, false); fill(rect(58, 22, 12, 84), COPPER); line(rect(58, 22, 12, 84), INK, 2.4); fill(rect(22, 58, 84, 12), COPPER); line(rect(22, 58, 84, 12), INK, 2.4); brass(circle(64, 64, 13)); fill(circle(64, 64, 5), INK); bolts(12, 12, 104, 104); });
    sprite("hatch", T, () -> { wood(rr(4, 4, 56, 56, 8)); fill(rr(13, 13, 38, 38, 5), c(0x3d2f30)); brass(poly(32, 16, 47, 32, 38, 32, 38, 47, 26, 47, 26, 32, 17, 32)); });
    sprite("pipe_core", T, () -> { solid(circle(32, 32, 15), c(0xd7ecef), c(0x8bb5bd), c(0x4c7480)); fill(circle(32, 32, 6), c(0x5aa9d6)); line(circle(32, 32, 6), INK, 2); });
    sprite("pipe_arm", T, () -> { fill(rect(30, 23, 34, 18), new GradientPaint(0, 23, c(0xcfe6ea), 0, 41, c(0x557f8b))); seg(30, 23, 64, 23, INK, 2.8); seg(30, 41, 64, 41, INK, 2.8); fill(rect(56, 20, 8, 24), new GradientPaint(0, 20, c(0xe6f4f6), 0, 44, c(0x55808c))); seg(57, 20, 64, 20, INK, 2.6); seg(57, 44, 64, 44, INK, 2.6); seg(57, 20, 57, 44, INK, 2.2); });
  }

  static void gathering() throws Exception {
    sprite("drill", T * 2, () -> { footing(2); fill(circle(64, 64, 45), c(0x4a3a36)); line(circle(64, 64, 45), INK, 3); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + Math.PI / 4; double x = 64 + Math.cos(an) * 52, y = 64 + Math.sin(an) * 52; copper(circle(x, y, 10)); bolt(x, y); } });
    sprite("drill_top", T * 2, () -> { copper(gear(64, 64, 12, 43, 36, 0)); for (int i = 0; i < 3; i++) { Shape b = turned(poly(64, 58, 98, 50, 92, 64, 98, 76, 64, 70), i * 2.0944, 64, 64); solid(b, IRON_L, IRON, IRON_D, 2.6); } brass(circle(64, 64, 14)); bolt(64, 64); });
    sprite("auger", T * 3, () -> { footing(3); fill(circle(96, 96, 72), c(0x3f3638)); line(circle(96, 96, 72), INK, 3.4); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + Math.PI / 4; double x = 96 + Math.cos(an) * 84, y = 96 + Math.sin(an) * 84; brass(circle(x, y, 15)); fill(circle(x, y, 6), IRON_D); } solid(rr(60, 4, 72, 18, 5), TEAL_L, TEAL, TEAL_D); solid(rr(60, 170, 72, 18, 5), TEAL_L, TEAL, TEAL_D); });
    sprite("auger_top", T * 3, () -> { brass(gear(96, 96, 18, 69, 60, 0)); for (int i = 0; i < 4; i++) { Path2D p = new Path2D.Double(); p.moveTo(96, 88); p.quadTo(126, 76, 152, 92); p.lineTo(148, 108); p.quadTo(120, 96, 96, 106); p.closePath(); solid(turned(p, i * Math.PI / 2, 96, 96), IRON_L, IRON, IRON_D, 3); } copper(circle(96, 96, 22)); bolts(88, 88, 16, 16); });
    sprite("tap", T, () -> { wood(circle(32, 32, 25)); for (int i = 1; i < 4; i++) line(circle(32, 32, i * 6), a(WOOD_D, 140), 2); solid(rr(24, 24, 16, 16, 5), c(0xffe08a), c(0xf2a52e), c(0xb8681c)); brass(rr(28, 6, 8, 22, 3)); });
    sprite("pump", T, () -> { solid(circle(32, 32, 28), TEAL_L, TEAL, TEAL_D); fill(circle(32, 32, 19), c(0x2f6a94)); line(circle(32, 32, 19), INK, 2.6); });
    sprite("pump_top", T, () -> { for (int i = 0; i < 3; i++) brass(turned(poly(32, 29, 47, 24, 49, 34, 32, 36), i * 2.0944, 32, 32)); brass(circle(32, 32, 6)); });
  }

  static void furnace(double x, double y, double w, double h) {
    iron(rr(x, y, w, h, 8));
    fill(rr(x + 6, y + 6, w - 12, h - 12, 5), c(0x2b1a1c));
    glow(x + w / 2, y + h / 2 + 4, Math.max(w, h) * .6, FIRE, 160);
    for (int i = 0; i < 4; i++) seg(x + 8 + i * (w - 16) / 3, y + 5, x + 8 + i * (w - 16) / 3, y + h - 5, IRON_D, 3);
  }
  static void brick(double x, double y, double w, double h, Color base) {
    Shape clip = g.getClip();
    g.clip(rr(x, y, w, h, 9));
    fill(rect(x, y, w, h), mix(base, INK, .35));
    int row = 0;
    for (double by = y; by < y + h; by += 11, row++)
      for (double bx = x - (row % 2) * 10; bx < x + w; bx += 21) fill(rr(bx + 1, by + 1, 19, 9, 2.5), mix(base, r.nextBoolean() ? Color.WHITE : INK, r.nextDouble() * .16));
    fill(rect(x, y, w, h), new LinearGradientPaint((float) x, (float) y, (float) (x + w), (float) (y + h), new float[]{0, 1}, new Color[]{a(Color.WHITE, 50), a(INK, 90)}));
    g.setClip(clip);
    line(rr(x, y, w, h, 9), INK, 3.2);
  }

  static void crafting() throws Exception {
    sprite("smelter", T * 2, () -> { brick(4, 4, 120, 120, c(0xb9624a)); fill(circle(64, 64, 40), c(0x3a2622)); line(circle(64, 64, 40), INK, 3); copper(ring(64, 64, 44, 34)); fill(circle(64, 64, 32), c(0x271512)); copper(circle(18, 18, 9)); copper(circle(110, 18, 9)); copper(circle(18, 110, 9)); copper(circle(110, 110, 9)); });
    sprite("smelter_hot", T * 2, () -> { fill(circle(64, 64, 32), new RadialGradientPaint(64f, 64f, 32f, new float[]{0f, .6f, 1f}, new Color[]{c(0xfff3b8), c(0xffa040), c(0xc23a1e)})); for (int i = 0; i < 5; i++) fill(blob(48 + r.nextInt(32), 48 + r.nextInt(32), 5, .3, 7, r), a(c(0xfff8d0), 180)); });
    sprite("glass_kiln", T * 2, () -> { brick(4, 4, 120, 120, c(0xcab69c)); cream(circle(64, 64, 46)); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; seg(64 + Math.cos(an) * 34, 64 + Math.sin(an) * 34, 64 + Math.cos(an) * 45, 64 + Math.sin(an) * 45, a(INK, 120), 2.4); } fill(circle(64, 64, 30), c(0x2d2226)); line(circle(64, 64, 30), INK, 3); teal(rr(46, 2, 36, 16, 5)); teal(rr(46, 110, 36, 16, 5)); });
    sprite("glass_kiln_hot", T * 2, () -> { fill(circle(64, 64, 28), new RadialGradientPaint(64f, 64f, 28f, new float[]{0f, .55f, 1f}, new Color[]{c(0xffffff), c(0xffd98a), c(0xe06a3a)})); fill(oval(57, 56, 9, 6), a(Color.WHITE, 190)); });
    sprite("gearworks", T * 3, () -> { footing(3); planks(16, 16, 160, 160, 8, false); fill(circle(72, 78, 46), a(c(0x3a2c2c), 220)); fill(circle(128, 128, 34), a(c(0x3a2c2c), 220)); line(circle(72, 78, 46), INK, 2.6); line(circle(128, 128, 34), INK, 2.6); copper(rr(118, 24, 52, 36, 8)); fill(rr(126, 32, 36, 20, 4), c(0x2b1a1c)); copper(rr(22, 138, 50, 32, 8)); bolts(124, 30, 40, 24); });
    sprite("gear_big", 92, () -> { brass(gear(46, 46, 12, 43, 35, 10)); for (int i = 0; i < 5; i++) { double an = i * 1.2566; fill(circle(46 + Math.cos(an) * 22, 46 + Math.sin(an) * 22, 6), a(INK, 190)); } });
    sprite("gear_small", 68, () -> { copper(gear(34, 34, 9, 31, 24, 8)); line(circle(34, 34, 16), a(INK, 110), 2.2); });
    sprite("ember_forge", T * 3, () -> { brick(4, 4, 184, 184, c(0x6d6478)); brass(ring(96, 96, 72, 52)); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; double x = 96 + Math.cos(an) * 62, y = 96 + Math.sin(an) * 62; fill(circle(x, y, 6), INK); fill(circle(x, y, 4), GLASS); } fill(circle(96, 96, 50), c(0x241820)); line(circle(96, 96, 50), INK, 3.4); for (double[] p : new double[][]{{22, 22}, {170, 22}, {22, 170}, {170, 170}}) { copper(circle(p[0], p[1], 14)); fill(circle(p[0], p[1], 6), INK); } });
    sprite("ember_forge_hot", T * 3, () -> { fill(circle(96, 96, 48), new RadialGradientPaint(96f, 96f, 48f, new float[]{0f, .5f, 1f}, new Color[]{c(0xfff0c0), c(0xff6a3a), c(0x7a1c3a)})); fill(star(96, 96, 6, 40, 16, 0), a(c(0xffe6a0), 190)); });
  }

  static void power() throws Exception {
    sprite("windmill", T * 2, () -> { stonefoot(2); wood(circle(64, 64, 34)); for (int i = 1; i < 4; i++) line(circle(64, 64, i * 9), a(WOOD_D, 130), 2); });
    sprite("windmill_top", 180, () -> { for (int i = 0; i < 4; i++) { AffineTransform t = g.getTransform(); g.rotate(i * Math.PI / 2, 90, 90); wood(rect(87, 8, 6, 82)); Path2D p = new Path2D.Double(); p.moveTo(93, 12); p.quadTo(120, 18, 117, 70); p.lineTo(93, 74); p.closePath(); solid(p, c(0xfffaf0), c(0xf0dfc2), c(0xc7a98a), 2.6); for (int k = 0; k < 3; k++) seg(94, 26 + k * 16, 115, 28 + k * 15, a(c(0xb0705a), 150), 2); g.setTransform(t); } copper(circle(90, 90, 13)); bolt(90, 90); });
    sprite("waterwheel", T * 2, () -> { stonefoot(2); fill(rr(12, 34, 104, 60, 8), c(0x2f5f7c)); line(rr(12, 34, 104, 60, 8), INK, 3); wood(rr(6, 22, 116, 14, 4)); wood(rr(6, 92, 116, 14, 4)); brass(circle(64, 29, 8)); brass(circle(64, 99, 8)); });
    for (int f = 0; f < 4; f++) { final int n = f; sprite("waterwheel_top" + f, T * 2, () -> { for (int i = 0; i < 9; i++) { double x = 14 + i * 12 - n * 3; if (x < 10 || x > 110) continue; double lit = Math.sin((x - 10) / 100 * Math.PI); solid(rect(x, 38, 8, 52), mix(WOOD_D, WOOD_L, lit), mix(WOOD_D, WOOD, lit), WOOD_D, 2); } iron(rr(58, 26, 12, 76, 4)); }); }
    sprite("sun_lens", T * 2, () -> { footing(2); copper(circle(64, 64, 48)); fill(circle(64, 64, 41), new RadialGradientPaint(50f, 48f, 60f, new float[]{0f, .6f, 1f}, new Color[]{c(0xf6fffb), c(0x9fe0d8), c(0x3f8f96)})); line(circle(64, 64, 41), INK, 2.6); fill(turned(oval(48, 46, 15, 7), -.7, 48, 46), a(Color.WHITE, 190)); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; brass(circle(64 + Math.cos(an) * 47, 64 + Math.sin(an) * 47, 5)); } });
    sprite("firebox", T * 2, () -> { iron(rr(4, 4, 120, 120, 14)); furnace(22, 22, 84, 60); copper(rr(20, 90, 88, 26, 8)); for (int i = 0; i < 4; i++) bolt(32 + i * 21, 103); copper(circle(104, 24, 14)); fill(circle(104, 24, 8), c(0x241820)); });
    sprite("pylon", T, () -> { wood(rr(14, 14, 36, 36, 9)); copper(circle(32, 32, 13)); cream(circle(32, 32, 7)); });
    sprite("tall_pylon", T * 2, () -> { stonefoot(2); wood(turned(rr(56, 14, 16, 100, 5), .78, 64, 64)); wood(turned(rr(56, 14, 16, 100, 5), -.78, 64, 64)); brass(circle(64, 64, 24)); cream(circle(64, 64, 13)); });
    sprite("flywheel", T * 2, () -> { footing(2); fill(circle(64, 64, 48), c(0x3a3038)); line(circle(64, 64, 48), INK, 3); });
    sprite("flywheel_top", T * 2, () -> { iron(ring(64, 64, 45, 30)); for (int i = 0; i < 6; i++) brass(turned(rect(60, 30, 8, 34), i * Math.PI / 3, 64, 64)); brass(circle(64, 64, 14)); bolt(64, 64); for (int i = 0; i < 6; i++) { double an = i * Math.PI / 3 + .5; fill(circle(64 + Math.cos(an) * 37, 64 + Math.sin(an) * 37, 3.4), BRASS_L); } });
  }

  static void defense() throws Exception {
    sprite("base1", T, () -> { stone(circle(32, 32, 28)); line(circle(32, 32, 20), a(INK, 120), 2.4); bolts(17, 17, 30, 30); });
    sprite("base2", T * 2, () -> { stonefoot(2); wood(circle(64, 64, 46)); line(circle(64, 64, 36), a(INK, 130), 3); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4 + .39; bolt(64 + Math.cos(an) * 41, 64 + Math.sin(an) * 41); } });
    sprite("base3", T * 3, () -> { stonefoot(3); copper(circle(96, 96, 74)); line(circle(96, 96, 58), a(INK, 130), 3); for (int i = 0; i < 12; i++) { double an = i * Math.PI / 6; bolt(96 + Math.cos(an) * 66, 96 + Math.sin(an) * 66); } });
    // Heads point to the right.
    sprite("slinger_top", T, () -> { wood(rr(28, 17, 32, 9, 3)); wood(rr(28, 38, 32, 9, 3)); copper(circle(29, 32, 16)); fill(circle(29, 32, 7), INK); fill(circle(28, 31, 4.6), COPPER_L); iron(rr(54, 15, 8, 13, 2)); iron(rr(54, 36, 8, 13, 2)); });
    sprite("firepot_top", T * 2, () -> { iron(rr(58, 46, 54, 36, 10)); copper(rr(98, 42, 18, 44, 6)); fill(oval(112, 64, 5, 14), c(0x241218)); iron(circle(52, 64, 34)); copper(ring(52, 64, 26, 16)); glow(52, 64, 18, FIRE, 220); fill(circle(52, 64, 8), FIRE_L); });
    sprite("boltcaster_top", T * 2, () -> { Path2D bow = new Path2D.Double(); bow.moveTo(78, 6); bow.curveTo(116, 34, 116, 94, 78, 122); line(bow, INK, 15); line(bow, BRASS, 9); line(bow, BRASS_L, 3); seg(78, 6, 42, 64, c(0xf7ecd0), 2.4); seg(78, 122, 42, 64, c(0xf7ecd0), 2.4); wood(rr(14, 53, 100, 22, 7)); iron(poly(104, 56, 124, 64, 104, 72)); copper(circle(36, 64, 15)); bolt(36, 64); });
    sprite("storm_bell_top", T * 2, () -> { teal(circle(64, 64, 38)); line(circle(64, 64, 28), a(INK, 120), 2.6); brass(circle(64, 64, 21)); fill(star(64, 64, 4, 18, 6, .78), c(0xe8fbff)); for (int i = 0; i < 6; i++) { double an = i * Math.PI / 3; cream(circle(64 + Math.cos(an) * 34, 64 + Math.sin(an) * 34, 6)); } });
    sprite("sun_lance_top", T * 3, () -> { brass(rr(80, 66, 92, 60, 14)); copper(rr(150, 72, 26, 48, 8)); fill(oval(174, 96, 8, 20), c(0xfff6d0)); line(oval(174, 96, 8, 20), INK, 3); copper(circle(78, 96, 48)); fill(circle(78, 96, 32), new RadialGradientPaint(68f, 84f, 44f, new float[]{0f, .6f, 1f}, new Color[]{c(0xfffdf0), c(0xffd27a), c(0xc26a2a)})); line(circle(78, 96, 32), INK, 3); fill(turned(oval(66, 82, 11, 6), -.7, 66, 82), a(Color.WHITE, 200)); for (int i = 0; i < 3; i++) seg(112 + i * 14, 68, 112 + i * 14, 124, a(INK, 110), 3); });
    sprite("mister_top", T, () -> { teal(circle(28, 32, 16)); fill(circle(28, 32, 9), c(0x5aa9d6)); line(circle(28, 32, 9), INK, 2.2); teal(rr(38, 25, 18, 14, 4)); brass(poly(54, 20, 63, 16, 63, 48, 54, 44)); });
    sprite("wall", T, () -> { copper(rr(3, 3, 58, 58, 8)); copper(rr(13, 13, 38, 38, 6)); bolts(9, 9, 46, 46); });
    sprite("wall_large", T * 2, () -> { copper(rr(3, 3, 122, 122, 12)); copper(rr(18, 18, 92, 92, 9)); seg(24, 24, 104, 104, a(INK, 90), 4); seg(104, 24, 24, 104, a(INK, 90), 4); copper(circle(64, 64, 15)); bolts(11, 11, 106, 106); });
    sprite("bronze_wall", T, () -> { brass(rr(3, 3, 58, 58, 8)); brass(poly(32, 10, 54, 32, 32, 54, 10, 32)); bolts(9, 9, 46, 46); });
    sprite("bronze_wall_large", T * 2, () -> { brass(rr(3, 3, 122, 122, 12)); brass(poly(64, 14, 114, 64, 64, 114, 14, 64)); brass(circle(64, 64, 20)); bolts(11, 11, 106, 106); bolt(64, 64); });
    sprite("tinker", T, () -> { wood(rr(5, 5, 54, 54, 12)); cream(circle(32, 32, 20)); fill(rr(28, 16, 8, 32, 2), c(0xd1503e)); fill(rr(16, 28, 32, 8, 2), c(0xd1503e)); bolts(10, 10, 44, 44); });
    for (int k = 1; k <= 3; k++) { final int n = k; sprite("crack" + k, T, () -> { for (int i = 0; i < n * 2; i++) { double x = 10 + r.nextInt(44), y = 10 + r.nextInt(44); for (int j = 0; j < 3 + n; j++) { double nx = x + r.nextInt(18) - 9, ny = y + r.nextInt(18) - 9; seg(x, y, nx, ny, a(c(0x1e1418), 190), 2.2); x = nx; y = ny; } } }); }
  }

  static void seasonal() throws Exception {
    sprite("brazier", T, () -> { stone(circle(32, 32, 26)); iron(circle(32, 32, 20)); fill(circle(32, 32, 14), c(0x2b1a1c)); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + .78; fill(circle(32 + Math.cos(an) * 8, 32 + Math.sin(an) * 8, 4), c(0x4a4656)); } });
    sprite("radiator", T * 2, () -> { footing(2); for (int i = 0; i < 6; i++) copper(rr(18 + i * 16, 18, 12, 92, 6)); brass(rr(12, 30, 104, 10, 5)); brass(rr(12, 88, 104, 10, 5)); });
    sprite("almanac", T * 3, () -> { footing(3); planks(16, 16, 160, 160, 6, true); cream(circle(96, 96, 70)); Color[] ks = {c(0x9ccb6a), c(0xf4cf5a), c(0xd8792f), c(0xb7d7ee)}; for (int i = 0; i < 4; i++) { Arc2D arc = new Arc2D.Double(34, 34, 124, 124, i * 90, 90, Arc2D.PIE); fill(arc, ks[i]); line(arc, INK, 2.4); } cream(circle(96, 96, 34)); for (int i = 0; i < 12; i++) { double an = i * Math.PI / 6; seg(96 + Math.cos(an) * 62, 96 + Math.sin(an) * 62, 96 + Math.cos(an) * 69, 96 + Math.sin(an) * 69, INK, 2.6); } });
    sprite("almanac_top", T * 3, () -> { brass(poly(96, 38, 104, 96, 96, 108, 88, 96)); brass(circle(96, 96, 10)); bolt(96, 96); });
    sprite("great_wheel", T * 5, () -> { double s = 320; stonefoot(5); fill(circle(160, 160, 142), c(0x3a3038)); line(circle(160, 160, 142), INK, 4); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + Math.PI / 4; double x = 160 + Math.cos(an) * 178, y = 160 + Math.sin(an) * 178; copper(circle(x, y, 26)); fill(circle(x, y, 13), INK); fill(circle(x, y, 10), new Color[]{c(0xf4cf5a), c(0xd8792f), c(0xb7d7ee), c(0x9ccb6a)}[i]); } });
    sprite("great_wheel_top", T * 5, () -> { brass(ring(160, 160, 138, 112)); for (int i = 0; i < 24; i++) { double an = i * Math.PI / 12; bolt(160 + Math.cos(an) * 125, 160 + Math.sin(an) * 125); } for (int i = 0; i < 8; i++) wood(turned(rr(154, 48, 12, 112, 4), i * Math.PI / 4, 160, 160)); copper(ring(160, 160, 64, 44)); brass(circle(160, 160, 34)); fill(star(160, 160, 8, 28, 12, 0), CREAM); });
    for (int k = 0; k < 4; k++) { final Color q = new Color[]{c(0x9ccb6a), c(0xf4cf5a), c(0xd8792f), c(0xb7d7ee)}[k]; sprite("spoke" + k, T * 5, () -> { Shape p = new Area(new Arc2D.Double(48, 48, 224, 224, 94 - 90 * 1, 82, Arc2D.PIE)); Area ar = new Area(p); ar.subtract(new Area(circle(160, 160, 66))); solid(ar, mix(q, Color.WHITE, .5), q, mix(q, INK, .25), 3); }); }
    sprite("hearth", T * 3, () -> { stonefoot(3); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + Math.PI / 4; double x = 96 + Math.cos(an) * 102, y = 96 + Math.sin(an) * 102; } copper(circle(96, 96, 78)); brick(44, 44, 104, 104, c(0xb9624a)); fill(circle(96, 96, 44), c(0x2b1a1c)); line(circle(96, 96, 44), INK, 4); for (int i = 0; i < 5; i++) { wood(turned(rr(62, 89, 68, 13, 6), i * .63, 96, 96)); } });
    sprite("hearth_ring", T * 3, () -> { brass(gear(96, 96, 24, 88, 80, 70)); for (int i = 0; i < 12; i++) { double an = i * Math.PI / 6; fill(circle(96 + Math.cos(an) * 75, 96 + Math.sin(an) * 75, 2.8), INK); } });
    for (int f = 0; f < 4; f++) sprite("flame" + f, 64, () -> { for (int k = 0; k < 3; k++) { double s = 1 - k * .3; double lean = r.nextDouble() * 10 - 5; Path2D p = new Path2D.Double(); double b = 52; p.moveTo(32 - 18 * s, b - 8); p.curveTo(32 - 20 * s, b - 26 * s, 32 - 4, b - 24 * s, 32 + lean, b - 46 * s); p.curveTo(32 + 8, b - 26 * s, 32 + 20 * s, b - 24 * s, 32 + 18 * s, b - 8); p.curveTo(32 + 12 * s, b + 4, 32 - 12 * s, b + 4, 32 - 18 * s, b - 8); fill(p, new Color[]{c(0xf0602a), c(0xffa23c), c(0xfff0a8)}[k]); } });
  }

  static void automata() throws Exception {
    sprite("toyworks", T * 3, () -> { footing(3); planks(16, 16, 160, 160, 7, true); solid(rr(50, 50, 92, 92, 14), TEAL_L, TEAL, TEAL_D); fill(rr(62, 62, 68, 68, 9), c(0x2d3a44)); line(rr(62, 62, 68, 68, 9), INK, 3); brass(rr(80, 140, 32, 44, 6)); fill(poly(88, 150, 104, 150, 96, 176), INK); brass(gear(36, 36, 8, 19, 14, 5)); brass(gear(156, 36, 8, 19, 14, 5)); });
    sprite("foundry", T * 4, () -> { brick(4, 4, 248, 248, c(0x8c5a52)); iron(rr(58, 58, 140, 140, 20)); fill(rr(76, 76, 104, 104, 14), c(0x2b1a1c)); line(rr(76, 76, 104, 104, 14), INK, 3.4); glow(128, 128, 60, FIRE, 90); for (double[] p : new double[][]{{34, 34}, {222, 34}, {34, 222}, {222, 222}}) { copper(circle(p[0], p[1], 22)); fill(circle(p[0], p[1], 11), c(0x241820)); } iron(rr(104, 196, 48, 54, 8)); fill(poly(114, 208, 142, 208, 128, 240), BRASS_L); });
    sprite("site", T, () -> { Shape clip = g.getClip(); g.clip(rr(2, 2, 60, 60, 6)); fill(rect(0, 0, T, T), a(c(0xe9d8b8), 70)); for (int i = -4; i < 10; i++) seg(i * 12, 64, i * 12 + 64, 0, a(c(0xffe9b0), 70), 4); g.setClip(clip); line(rr(3, 3, 58, 58, 6), a(c(0xffe9b0), 190), 2.6); });
    // Bramble.
    sprite("nest", T * 3, () -> { for (int i = 0; i < 22; i++) { double an = i * .2856, x = 96, y = 96; double w = 13; for (int k = 0; k < 6; k++) { double nx = x + Math.cos(an) * 15, ny = y + Math.sin(an) * 15; seg(x, y, nx, ny, INK, w + 4); seg(x, y, nx, ny, mix(THORN_D, LEAF_D, (i % 3) * .3), w); x = nx; y = ny; w *= .8; an += (r.nextDouble() - .5) * .9; if (k > 2) { double sa = an + 1.4; Shape th = poly(nx - 3, ny - 3, nx + Math.cos(sa) * 11, ny + Math.sin(sa) * 11, nx + 3, ny + 3); fill(th, c(0xd9c69a)); line(th, INK, 1.8); } } } solid(blob(96, 96, 48, .1, 12, r), c(0xb0657e), c(0x7c3a5c), c(0x48203e), 4); for (int i = 0; i < 7; i++) { double an = i * .9; solid(blob(96 + Math.cos(an) * 28, 96 + Math.sin(an) * 28, 9, .2, 7, r), c(0xd88aa0), c(0xa84a70), c(0x6a2a50), 2.4); } });
    sprite("nest_heart", 80, () -> { solid(blob(40, 40, 24, .12, 10, r), c(0xffd27a), c(0xf06a4a), c(0x9c2440), 3); fill(oval(33, 33, 7, 5), a(Color.WHITE, 150)); });
    sprite("thorn", T * 2, () -> { for (int i = 0; i < 9; i++) { double an = i * .698; Shape th = turned(poly(64, 54, 118, 64, 64, 74), an, 64, 64); solid(th, c(0xa88a6a), BARK, BARK_D, 3); } solid(blob(64, 64, 34, .12, 10, r), LEAF_L, LEAF, LEAF_D, 3.4); });
    sprite("thorn_top", T * 2, () -> { solid(poly(48, 46, 120, 64, 48, 82), c(0xf4e6b8), c(0xd9c08a), c(0x94784a), 3); solid(circle(54, 64, 22), c(0xd88aa0), c(0x9c446a), c(0x5c2448), 3); fill(circle(58, 64, 7), INK); fill(circle(60, 62, 2.6), c(0xffe6a0)); });
  }

  static void eye(double x, double y, double s) { fill(circle(x, y, s + 1), INK); fill(circle(x, y, s), c(0xffd84a)); fill(circle(x + s * .3, y, s * .45), INK); }
  static void eyes(double x, double y, double gap, double s) { eye(x, y - gap, s); eye(x, y + gap, s); }

  // Creatures face to the right.
  static void creatures() throws Exception {
    sprite("u_lantern", T, () -> { solid(oval(32, 32, 20, 22), c(0xffe9b8), c(0xf6b45a), c(0xc46a34)); for (int i = -1; i <= 1; i++) { Path2D p = new Path2D.Double(); p.moveTo(32 + i * 10, 11 + Math.abs(i) * 3); p.quadTo(32 + i * 15, 32, 32 + i * 10, 53 - Math.abs(i) * 3); line(p, a(c(0x8c3a24), 170), 2.2); } wood(rr(22, 5, 20, 9, 4)); wood(rr(22, 50, 20, 9, 4)); fill(circle(38, 26, 3), INK); fill(circle(38, 38, 3), INK); fill(circle(39, 25, 1.1), Color.WHITE); fill(circle(39, 37, 1.1), Color.WHITE); });
    sprite("wing_paper", 56, 40, () -> { Path2D p = new Path2D.Double(); p.moveTo(8, 34); p.curveTo(10, 8, 34, 0, 52, 10); p.curveTo(44, 18, 48, 26, 40, 34); p.closePath(); solid(p, c(0xfffaf0), c(0xf3dfba), c(0xc7a072), 2.6); seg(10, 33, 48, 12, a(c(0x9c6a40), 140), 1.8); seg(12, 34, 42, 24, a(c(0x9c6a40), 140), 1.8); });
    sprite("u_beetle", T, () -> { copper(oval(28, 32, 20, 17)); seg(8, 32, 46, 32, INK, 2.6); teal(circle(45, 32, 11)); eyes(50, 32, 5, 2.6); brass(rr(48, 29, 14, 6, 2)); for (int i = 0; i < 3; i++) { bolt(18 + i * 9, 23); bolt(18 + i * 9, 41); } });
    sprite("u_kite", T, () -> { solid(poly(60, 32, 26, 6, 10, 32, 26, 58), c(0xfff6e0), c(0xe86a52), c(0x9c3434)); solid(poly(60, 32, 26, 6, 22, 32), c(0xbff0ea), TEAL, TEAL_D, 2.4); seg(26, 6, 26, 58, INK, 2.6); seg(10, 32, 60, 32, INK, 2.6); brass(circle(26, 32, 6)); });
    sprite("u_golem", 84, () -> { iron(rr(14, 4, 34, 20, 8)); iron(rr(14, 60, 34, 20, 8)); copper(rr(12, 14, 52, 56, 14)); furnace(26, 28, 28, 28); brass(circle(62, 42, 13)); eyes(67, 42, 5, 2.8); iron(rr(44, 4, 26, 13, 5)); iron(rr(44, 67, 26, 13, 5)); });
    sprite("u_lanternfly", 84, () -> { brass(oval(30, 42, 24, 12)); for (int i = 0; i < 3; i++) seg(16 + i * 10, 32, 16 + i * 10, 52, a(INK, 130), 2.2); glow(16, 42, 14, FIRE_L, 230); copper(circle(56, 42, 15)); eyes(63, 42, 7, 3.4); });
    sprite("wing_glass", 72, 48, () -> { Path2D p = new Path2D.Double(); p.moveTo(6, 42); p.curveTo(14, 6, 46, -4, 68, 12); p.curveTo(62, 32, 36, 46, 6, 42); fill(p, a(GLASS, 150)); line(p, INK, 2.4); seg(8, 41, 60, 14, a(TEAL_D, 160), 1.8); seg(14, 40, 48, 30, a(TEAL_D, 160), 1.6); });
    sprite("u_burr", T, () -> { for (int i = 0; i < 14; i++) { Shape th = turned(poly(32, 28, 62, 32, 32, 36), i * .4488, 32, 32); solid(th, c(0xc9b27a), c(0x9c7a4a), c(0x5c4030), 1.8); } solid(circle(32, 32, 18), LEAF_L, c(0x8a8a3e), c(0x4f5a2c)); eyes(38, 32, 6, 3.4); });
    sprite("u_nettle", T, () -> { for (int i = 0; i < 5; i++) { Shape l = turned(poly(30, 32, 10, 22, -2, 32, 10, 42), Math.PI + (i - 2) * .62 + Math.PI, 32, 32); solid(l, LEAF_L, LEAF, LEAF_D, 2.2); } solid(circle(34, 32, 15), c(0xc6f06a), c(0x7fb23e), c(0x3f6a2c)); solid(poly(44, 27, 62, 32, 44, 37), c(0xf6ffc0), c(0xcde86a), c(0x7f9c34), 2.2); eyes(40, 32, 6, 3); });
    sprite("u_stumper", 84, () -> { solid(circle(40, 42, 31), BARK_L, BARK, BARK_D, 3.6); solid(circle(38, 42, 22), c(0xe9c894), c(0xc99a64), c(0x8f6840), 2.6); for (int i = 1; i < 4; i++) line(circle(38, 42, i * 5), a(BARK, 170), 2); solid(poly(60, 24, 82, 34, 64, 38), CREAM, c(0xd9c08a), c(0x94784a), 2.2); solid(poly(60, 60, 82, 50, 64, 46), CREAM, c(0xd9c08a), c(0x94784a), 2.2); eyes(62, 42, 7, 3.4); for (int i = 0; i < 4; i++) fill(blob(14 + r.nextInt(14), 14 + r.nextInt(52), 4, .3, 7, r), LEAF); });
    sprite("u_dandel", T, () -> { for (int i = 0; i < 18; i++) { double an = i * .349; seg(32, 32, 32 + Math.cos(an) * 25, 32 + Math.sin(an) * 25, a(c(0xfffdf0), 190), 1.6); fill(circle(32 + Math.cos(an) * 26, 32 + Math.sin(an) * 26, 3), a(Color.WHITE, 230)); } for (int i = 0; i < 10; i++) { double an = i * .628 + .2; fill(circle(32 + Math.cos(an) * 16, 32 + Math.sin(an) * 16, 2.6), a(Color.WHITE, 220)); } solid(circle(32, 32, 10), c(0xd9b27a), c(0x9c7444), c(0x5c4030), 2.6); eyes(36, 32, 4, 2.4); });
    sprite("u_hornwing", T, () -> { solid(oval(26, 32, 18, 11), c(0xffd27a), c(0xd98a2e), c(0x8c4a20)); for (int i = 0; i < 3; i++) seg(16 + i * 8, 23, 16 + i * 8, 41, INK, 3.4); solid(poly(12, 28, -2, 32, 12, 36), INK, INK, INK, 1); solid(circle(44, 32, 11), c(0xd88aa0), THORN, THORN_D); seg(50, 26, 61, 20, INK, 3); seg(50, 38, 61, 44, INK, 3); eyes(49, 32, 5, 3); });
    sprite("wing_leaf", 56, 40, () -> { Path2D p = new Path2D.Double(); p.moveTo(6, 34); p.curveTo(12, 6, 36, -2, 54, 10); p.curveTo(46, 30, 26, 38, 6, 34); solid(p, c(0xe6f6a8), c(0xa4c85a), c(0x5c8034), 2.2); seg(7, 34, 48, 13, a(LEAF_D, 190), 1.8); });
    sprite("u_creepvine", 76, () -> { for (int i = 0; i < 3; i++) solid(circle(18 + i * 15, 38, 15 - Math.abs(i - 1) * 2), c(0x8fc27a), c(0x4f8a52), c(0x2c5540)); solid(circle(56, 38, 16), c(0xd88aa0), c(0x9c446a), c(0x5c2448)); for (int i = 0; i < 5; i++) { double an = -.8 + i * .4; Shape th = turned(poly(66, 35, 76, 38, 66, 41), an, 56, 38); fill(th, CREAM); line(th, INK, 1.6); } eyes(60, 38, 6, 3.2); });
    sprite("u_podder", 88, () -> { solid(circle(44, 44, 34), c(0xd8a0d0), c(0x9c5c9c), c(0x5a3068), 3.6); for (int i = 0; i < 6; i++) { double an = i * 1.047 + .4; double x = 40 + Math.cos(an) * 19, y = 44 + Math.sin(an) * 19; solid(circle(x, y, 9), c(0xe9d06a), c(0xb09a3c), c(0x6a5a2c), 2.4); fill(circle(x - 2, y - 2, 2.6), a(Color.WHITE, 150)); } eyes(72, 44, 8, 3.8); });
    sprite("u_gnarl", 150, () -> { for (int i = 0; i < 12; i++) { double an = i * .5236; Shape th = turned(poly(75, 66, 144, 75, 75, 84), an, 75, 75); solid(th, BARK_L, BARK, BARK_D, 2.8); } solid(blob(75, 75, 52, .08, 13, r), BARK_L, BARK, BARK_D, 4.4); for (int i = 1; i < 5; i++) line(blob(72, 75, i * 9.5, .06, 11, r), a(BARK_D, 190), 2.8); for (int i = 0; i < 16; i++) { double an = r.nextDouble() * 6.28, d = 32 + r.nextInt(18); fill(blob(75 + Math.cos(an) * d, 75 + Math.sin(an) * d, 6 + r.nextInt(4), .3, 8, r), a(LEAF, 220)); } eyes(106, 75, 14, 7); solid(poly(112, 66, 138, 75, 112, 84), c(0x2b1a1c), c(0x2b1a1c), c(0x2b1a1c), 3); });
    sprite("u_rimewood", 190, () -> { for (int i = 0; i < 14; i++) { double an = i * .4488; Shape th = turned(poly(95, 82, 186, 95, 95, 108), an, 95, 95); solid(th, Color.WHITE, c(0xb7d7ee), c(0x5a86b0), 3); } solid(blob(95, 95, 64, .08, 14, r), c(0xdfeefa), c(0x7a98b8), c(0x3d5478), 5); for (int i = 1; i < 5; i++) line(blob(92, 95, i * 12, .06, 11, r), a(c(0x2f4468), 170), 3.2); for (int i = 0; i < 12; i++) { double an = r.nextDouble() * 6.28, d = 30 + r.nextInt(30); fill(star(95 + Math.cos(an) * d, 95 + Math.sin(an) * d, 6, 9, 3, r.nextDouble()), a(Color.WHITE, 220)); } for (int k = -1; k <= 1; k += 2) { fill(circle(134, 95 + k * 18, 10), INK); fill(circle(134, 95 + k * 18, 8), c(0x8ff0ff)); fill(circle(137, 95 + k * 18, 3.6), INK); } solid(poly(144, 84, 176, 95, 144, 106), c(0x16233c), c(0x16233c), c(0x16233c), 3); });
    Object[][] legs = {{"brass", BRASS_L, BRASS, BRASS_D}, {"vine", LEAF_L, LEAF, LEAF_D}, {"wood", BARK_L, BARK, BARK_D}, {"ice", c(0xf4fbff), c(0x9fc4e0), c(0x4f78a0)}};
    for (Object[] l : legs) { sprite("leg_" + l[0], 64, 16, () -> { solid(rr(2, 4, 60, 8, 4), (Color) l[1], (Color) l[2], (Color) l[3], 2.4); fill(circle(6, 8, 3), INK); }); }
  }

  static void shots() throws Exception {
    sprite("shot_pebble", 32, () -> solid(circle(16, 16, 9), COPPER_L, COPPER, COPPER_D, 2.4));
    sprite("shot_shard", 32, () -> solid(poly(28, 16, 12, 9, 5, 16, 12, 23), Color.WHITE, c(0xd8c2a0), c(0x7b6a55), 2.2));
    sprite("shot_slug", 32, () -> solid(rr(6, 10, 21, 12, 6), BRASS_L, BRASS, BRASS_D, 2.2));
    sprite("shot_pot", 40, () -> { solid(circle(20, 20, 13), IRON_L, IRON, IRON_D); fill(circle(20, 20, 6), FIRE); fill(circle(20, 20, 3), FIRE_L); });
    sprite("shot_bolt", 64, 24, () -> { wood(rr(6, 9, 44, 6, 2)); iron(poly(46, 4, 62, 12, 46, 20)); solid(poly(2, 4, 14, 10, 14, 14, 2, 20), Color.WHITE, c(0xe86a52), c(0x9c3434), 2); });
    sprite("shot_spark", 32, () -> { glow(16, 16, 15, FIRE_L, 200); fill(oval(16, 16, 10, 5), c(0xfff3c0)); });
    sprite("shot_sting", 32, () -> solid(poly(29, 16, 8, 11, 3, 16, 8, 21), c(0xf6ffc0), c(0xb8e05a), c(0x5c8c34), 2));
    sprite("shot_thorn", 40, () -> solid(poly(38, 20, 12, 12, 4, 20, 12, 28), c(0xfff0c8), c(0xc9a86a), c(0x6b4a3a), 2.4));
    sprite("shot_seed", 32, () -> { solid(oval(16, 20, 5, 7), c(0xd9b27a), c(0x9c7444), c(0x5c4030), 2); for (int i = 0; i < 5; i++) seg(16, 14, 8 + i * 4, 4, a(Color.WHITE, 220), 1.6); });
    sprite("shot_pod", 40, () -> { solid(circle(20, 20, 13), c(0xe0b0d8), c(0x9c5c9c), c(0x5a3068)); fill(circle(20, 20, 5), c(0xe9d06a)); });
    sprite("shot_rime", 48, () -> { glow(24, 24, 23, c(0xbfeaff), 170); solid(star(24, 24, 6, 19, 8, 0), Color.WHITE, c(0xbfe4fa), c(0x5a9ac8), 2.4); });
  }

  static void effects() throws Exception {
    sprite("glow", 128, () -> fill(circle(64, 64, 64), new RadialGradientPaint(64f, 64f, 64f, new float[]{0f, .25f, .6f, 1f}, new Color[]{a(Color.WHITE, 255), a(Color.WHITE, 140), a(Color.WHITE, 34), a(Color.WHITE, 0)})));
    sprite("shadow", 64, () -> fill(circle(32, 32, 32), new RadialGradientPaint(32f, 32f, 32f, new float[]{0f, .6f, 1f}, new Color[]{a(Color.BLACK, 255), a(Color.BLACK, 170), a(Color.BLACK, 0)})));
    sprite("puff", 64, () -> { for (int i = 0; i < 9; i++) { double an = i * .7, d = i == 0 ? 0 : 14; double x = 32 + Math.cos(an) * d, y = 32 + Math.sin(an) * d; fill(circle(x, y, 14), new RadialGradientPaint((float) x, (float) y, 14f, new float[]{0f, .7f, 1f}, new Color[]{a(Color.WHITE, 120), a(Color.WHITE, 80), a(Color.WHITE, 0)})); } });
    sprite("ring", 128, () -> { line(circle(64, 64, 58), Color.WHITE, 4); line(circle(64, 64, 58), a(Color.WHITE, 60), 10); });
    sprite("dash", 32, 8, () -> fill(rr(0, 1, 32, 6, 3), Color.WHITE));
    sprite("spark", 32, 12, () -> fill(new Ellipse2D.Double(0, 0, 32, 12), new RadialGradientPaint(16f, 6f, 16f, new float[]{0f, .5f, 1f}, new Color[]{Color.WHITE, a(Color.WHITE, 170), a(Color.WHITE, 0)})));
    sprite("chip", 16, () -> fill(poly(8, 1, 15, 7, 10, 15, 2, 11), Color.WHITE));
    sprite("leaf", 24, 16, () -> { Path2D p = new Path2D.Double(); p.moveTo(1, 8); p.quadTo(10, -3, 22, 8); p.quadTo(10, 19, 1, 8); fill(p, Color.WHITE); seg(1, 8, 20, 8, a(Color.BLACK, 70), 1.6); });
    sprite("drop", 6, 32, () -> fill(rr(1, 0, 4, 32, 2), new GradientPaint(0, 0, a(Color.WHITE, 0), 0, 32, a(Color.WHITE, 220))));
    sprite("flake", 24, () -> { for (int i = 0; i < 3; i++) line(turned(new Line2D.Double(3, 12, 21, 12), i * Math.PI / 3, 12, 12), Color.WHITE, 2); fill(circle(12, 12, 3), Color.WHITE); });
    sprite("petal", 16, () -> fill(turned(oval(8, 8, 6, 3), .6, 8, 8), Color.WHITE));
    sprite("mote", 16, () -> fill(circle(8, 8, 8), new RadialGradientPaint(8f, 8f, 8f, new float[]{0f, 1f}, new Color[]{Color.WHITE, a(Color.WHITE, 0)})));
    sprite("frost", T, () -> { for (int c0 = 0; c0 < 4; c0++) { double x = c0 % 2 == 0 ? 4 : 60, y = c0 < 2 ? 4 : 60; double to = Math.atan2(32 - y, 32 - x); for (int i = 0; i < 5; i++) { double an = to + (i - 2) * .3, l = 12 + r.nextInt(13); double nx = x + Math.cos(an) * l, ny = y + Math.sin(an) * l; seg(x, y, nx, ny, a(c(0xe8f6ff), 230), 2.6); for (int k = 1; k < 3; k++) { double bx = x + Math.cos(an) * l * k / 3, by = y + Math.sin(an) * l * k / 3; seg(bx, by, bx + Math.cos(an + .8) * 6, by + Math.sin(an + .8) * 6, a(c(0xe8f6ff), 200), 1.8); seg(bx, by, bx + Math.cos(an - .8) * 6, by + Math.sin(an - .8) * 6, a(c(0xe8f6ff), 200), 1.8); } } } });
    sprite("bolt_icon", 32, () -> solid(poly(19, 2, 6, 18, 15, 18, 12, 30, 26, 13, 17, 13), c(0xfff6b0), c(0xffd23c), c(0xd08a1e), 2.6));
    sprite("drop_icon", 32, () -> { Path2D p = new Path2D.Double(); p.moveTo(16, 3); p.curveTo(28, 17, 27, 29, 16, 29); p.curveTo(5, 29, 4, 17, 16, 3); solid(p, c(0xd8f2ff), c(0x5ab4e8), c(0x2c6aa8), 2.6); });
    sprite("ammo_icon", 32, () -> { for (int i = 0; i < 3; i++) solid(rr(5 + i * 8, 8, 6, 18, 3), BRASS_L, BRASS, BRASS_D, 2); });
    sprite("arrow", 48, () -> { Shape p = poly(10, 8, 38, 24, 10, 40, 17, 24); fill(p, Color.WHITE); line(p, a(INK, 190), 3); });
    sprite("corner", 24, () -> { Path2D p = new Path2D.Double(); p.moveTo(3, 21); p.lineTo(3, 3); p.lineTo(21, 3); line(p, INK, 7); line(p, Color.WHITE, 4); });
    sprite("cross", 48, () -> { seg(10, 10, 38, 38, INK, 11); seg(38, 10, 10, 38, INK, 11); seg(10, 10, 38, 38, c(0xf0604a), 6); seg(38, 10, 10, 38, c(0xf0604a), 6); });
    sprite("wire", 32, 10, () -> { fill(rect(0, 3, 32, 4), Color.WHITE); fill(rect(0, 1, 32, 8), a(Color.WHITE, 70)); });
    sprite("beam", 32, 32, () -> fill(rect(0, 0, 32, 32), new LinearGradientPaint(0f, 0f, 0f, 32f, new float[]{0f, .35f, .5f, .65f, 1f}, new Color[]{a(Color.WHITE, 0), a(Color.WHITE, 120), Color.WHITE, a(Color.WHITE, 120), a(Color.WHITE, 0)})));
    sprite("white", 8, () -> fill(rect(0, 0, 8, 8), Color.WHITE));
    sprite("vignette", 64, () -> fill(rect(0, 0, 64, 64), new RadialGradientPaint(32f, 32f, 46f, new float[]{0f, .55f, 1f}, new Color[]{a(Color.BLACK, 0), a(Color.BLACK, 0), a(Color.BLACK, 200)})));
  }

  static void icon(String name, Draw d) throws Exception { sprite("icon_" + name, 48, d); }
  static void ui() throws Exception {
    // Nine slices, 16 pixels of border.
    sprite("panel", T, () -> { solid(rr(2, 2, 60, 60, 10), c(0x8a5f43), c(0x6b4634), c(0x4b3029), 3); fill(rr(8, 8, 48, 48, 6), c(0x3a2a2c)); line(rr(8, 8, 48, 48, 6), a(c(0xe0b070), 130), 1.6); });
    sprite("paper", T, () -> { solid(rr(2, 2, 60, 60, 9), c(0xf9ecd0), c(0xf0dcb4), c(0xd9bd8c), 3); line(rr(7, 7, 50, 50, 5), a(c(0x9c6a40), 110), 1.6); });
    sprite("button", T, () -> { solid(rr(2, 2, 60, 60, 10), c(0xe0a46c), c(0xb9784a), c(0x8c5434), 3); line(rr(7, 7, 50, 50, 7), a(c(0xffe0b0), 120), 1.6); });
    sprite("button_hot", T, () -> { solid(rr(2, 2, 60, 60, 10), c(0xffcf8c), c(0xdc9a58), c(0xae6c3e), 3); line(rr(7, 7, 50, 50, 7), a(c(0xfff2d0), 190), 1.8); });
    sprite("button_off", T, () -> { solid(rr(2, 2, 60, 60, 10), c(0x7c6a62), c(0x5f5050), c(0x473c40), 3); });
    sprite("slot", T, () -> { fill(rr(3, 3, 58, 58, 9), c(0x2c2023)); line(rr(3, 3, 58, 58, 9), c(0x7c573f), 3); });
    sprite("slot_hot", T, () -> { fill(rr(3, 3, 58, 58, 9), c(0x4d3734)); line(rr(3, 3, 58, 58, 9), c(0xf0c070), 4); });
    icon("haul", () -> { copper(rr(4, 16, 40, 16, 5)); for (int i = 0; i < 3; i++) { Path2D p = new Path2D.Double(); p.moveTo(11 + i * 11, 19); p.lineTo(16 + i * 11, 24); p.lineTo(11 + i * 11, 29); line(p, CREAM, 3); } });
    icon("gather", () -> { seg(12, 38, 34, 14, INK, 9); seg(12, 38, 34, 14, WOOD_L, 5); Path2D p = new Path2D.Double(); p.moveTo(18, 6); p.quadTo(36, 8, 42, 28); line(p, INK, 10); line(p, STONE_L, 6); });
    icon("craft", () -> { iron(poly(6, 14, 34, 14, 44, 20, 32, 24, 30, 32, 38, 40, 10, 40, 18, 32, 16, 24)); fill(circle(36, 9, 3.5), FIRE); fill(circle(29, 6, 2.4), FIRE_L); });
    icon("power", () -> solid(poly(28, 3, 10, 27, 22, 27, 18, 45, 38, 20, 26, 20), c(0xfff6b0), c(0xffd23c), c(0xd08a1e), 3));
    icon("guard", () -> { Path2D p = new Path2D.Double(); p.moveTo(24, 4); p.lineTo(42, 10); p.curveTo(42, 28, 36, 38, 24, 45); p.curveTo(12, 38, 6, 28, 6, 10); p.closePath(); solid(p, COPPER_L, COPPER, COPPER_D); seg(24, 10, 24, 38, CREAM, 4); seg(13, 20, 35, 20, CREAM, 4); });
    icon("season", () -> { Color[] ks = {c(0x9ccb6a), c(0xf4cf5a), c(0xd8792f), c(0xb7d7ee)}; for (int i = 0; i < 4; i++) { Arc2D arc = new Arc2D.Double(5, 5, 38, 38, i * 90, 90, Arc2D.PIE); fill(arc, ks[i]); } line(circle(24, 24, 19), INK, 3); seg(5, 24, 43, 24, INK, 2); seg(24, 5, 24, 43, INK, 2); cream(circle(24, 24, 6)); });
    icon("crew", () -> { copper(oval(20, 26, 16, 14)); teal(circle(36, 26, 8)); for (int i = 0; i < 3; i++) { seg(12 + i * 8, 12, 9 + i * 8, 6, INK, 3); seg(12 + i * 8, 40, 9 + i * 8, 46, INK, 3); } });
    icon("remove", () -> { seg(13, 13, 35, 35, INK, 12); seg(35, 13, 13, 35, INK, 12); seg(13, 13, 35, 35, c(0xf0604a), 7); seg(35, 13, 13, 35, c(0xf0604a), 7); });
    icon("learn", () -> { cream(poly(4, 10, 24, 14, 24, 42, 4, 38)); cream(poly(44, 10, 24, 14, 24, 42, 44, 38)); for (int i = 0; i < 3; i++) { seg(9, 18 + i * 6, 19, 20 + i * 6, a(INK, 130), 2); seg(29, 20 + i * 6, 39, 18 + i * 6, a(INK, 130), 2); } });
    icon("pause", () -> { cream(rr(11, 8, 10, 32, 4)); cream(rr(27, 8, 10, 32, 4)); });
    icon("play", () -> cream(poly(12, 7, 41, 24, 12, 41)));
    icon("fast", () -> { cream(poly(5, 9, 25, 24, 5, 39)); cream(poly(24, 9, 44, 24, 24, 39)); });
    icon("menu", () -> { for (int i = 0; i < 3; i++) cream(rr(7, 9 + i * 12, 34, 7, 3)); });
    icon("rotate", () -> { line(new Arc2D.Double(9, 9, 30, 30, 40, 280, Arc2D.OPEN), INK, 9); line(new Arc2D.Double(9, 9, 30, 30, 40, 280, Arc2D.OPEN), CREAM, 5); cream(poly(30, 4, 46, 12, 32, 22)); });
    icon("copy", () -> { cream(rr(6, 6, 24, 28, 4)); cream(rr(17, 14, 24, 28, 4)); });
    icon("crewmode", () -> { for (double[] p : new double[][]{{14, 15}, {34, 15}, {24, 34}}) brass(circle(p[0], p[1], 8)); });
    icon("skull", () -> { solid(circle(24, 24, 18), c(0xd88aa0), THORN, THORN_D); for (int i = 0; i < 10; i++) { double an = i * .628; seg(24 + Math.cos(an) * 18, 24 + Math.sin(an) * 18, 24 + Math.cos(an) * 23, 24 + Math.sin(an) * 23, INK, 4); } eye(18, 23, 4); eye(31, 23, 4); });
    icon("lock", () -> { line(new Arc2D.Double(14, 5, 20, 26, 0, 180, Arc2D.OPEN), INK, 8); line(new Arc2D.Double(14, 5, 20, 26, 0, 180, Arc2D.OPEN), STONE_L, 4); brass(rr(9, 19, 30, 24, 5)); fill(circle(24, 29, 4), INK); fill(rect(22.4, 30, 3.2, 8), INK); });
    icon("check", () -> { Path2D p = new Path2D.Double(); p.moveTo(9, 25); p.lineTo(20, 36); p.lineTo(40, 11); line(p, INK, 11); line(p, c(0x9ce07a), 6); });
    icon("wave", () -> { solid(circle(24, 24, 17), LEAF_L, LEAF, LEAF_D); for (int i = 0; i < 9; i++) { Shape th = turned(poly(38, 20, 47, 24, 38, 28), i * .698, 24, 24); solid(th, CREAM, c(0xd9c08a), c(0x94784a), 1.8); } });
    for (int i = 0; i < 4; i++) { final int n = i; sprite("season" + i, 64, () -> {
      if (n == 0) { for (int p = 0; p < 5; p++) { double an = p * 1.2566 - 1.57; solid(circle(32 + Math.cos(an) * 14, 32 + Math.sin(an) * 14, 11), c(0xffeaf2), c(0xf7b6d0), c(0xd078a0), 2.6); } solid(circle(32, 32, 8), c(0xfff3a8), c(0xf2c53a), c(0xb8841e), 2.6); }
      if (n == 1) { for (int p = 0; p < 10; p++) { Shape ray = turned(poly(50, 27, 63, 32, 50, 37), p * .628, 32, 32); solid(ray, c(0xfff3a8), c(0xf4b83a), c(0xd0801e), 2.2); } solid(circle(32, 32, 17), c(0xfffbd0), c(0xffd84a), c(0xe89a2a), 3); }
      if (n == 2) { Path2D p = new Path2D.Double(); p.moveTo(10, 54); p.curveTo(8, 22, 28, 8, 56, 8); p.curveTo(56, 36, 42, 56, 10, 54); solid(p, c(0xffc06a), c(0xd8702f), c(0x8c3a24), 3); seg(12, 52, 46, 18, INK, 2.8); seg(26, 38, 28, 24, INK, 2.2); seg(26, 38, 42, 38, INK, 2.2); }
      if (n == 3) { for (int p = 0; p < 6; p++) { double an = p * 1.047; for (int k = 0; k < 2; k++) { Color q = k == 0 ? INK : c(0xe8f6ff); double w = k == 0 ? 8 : 4.4; double x = 32 + Math.cos(an) * 26, y = 32 + Math.sin(an) * 26; seg(32, 32, x, y, q, w); double bx = 32 + Math.cos(an) * 16, by = 32 + Math.sin(an) * 16; seg(bx, by, bx + Math.cos(an + .8) * 8, by + Math.sin(an + .8) * 8, q, w * .8); seg(bx, by, bx + Math.cos(an - .8) * 8, by + Math.sin(an - .8) * 8, q, w * .8); } } }
    }); }
    sprite("title", 900, 190, () -> {
      Font f = new Font("Serif", Font.BOLD, 132);
      var gv = f.createGlyphVector(g.getFontRenderContext(), "Hearthwheel");
      Shape t = gv.getOutline(0, 0);
      Rectangle2D b = t.getBounds2D();
      t = AffineTransform.getTranslateInstance(450 - b.getCenterX(), 95 - b.getCenterY()).createTransformedShape(t);
      g.translate(0, 6); line(t, a(c(0x1c1218), 200), 22); g.translate(0, -6);
      line(t, INK, 18); line(t, c(0x8c4a2c), 11);
      Rectangle2D tb = t.getBounds2D();
      fill(t, new LinearGradientPaint(0f, (float) tb.getMinY(), 0f, (float) tb.getMaxY(), new float[]{0f, .5f, 1f}, new Color[]{c(0xfff3c8), c(0xffc865), c(0xe07f3c)}));
    });
  }

  static final Color STEEL = c(0xa8b8c8), STEEL_D = c(0x62748a), STEEL_L = c(0xe8f2fa), STAR = c(0xb48af0), STAR_D = c(0x6a4ab0), STAR_L = c(0xe8d8ff);
  static void steel(Shape s) { solid(s, STEEL_L, STEEL, STEEL_D); }
  static void starry(Shape s) { solid(s, STAR_L, STAR, STAR_D); }

  static void goods2() throws Exception {
    int s = 48;
    sprite("item_iron", s, () -> { for (double[] p : new double[][]{{16, 31, 10}, {33, 30, 11}, {24, 17, 11}}) solid(blob(p[0], p[1], p[2], .2, 6, r), c(0xd8a898), c(0x8f645c), c(0x4c3434), 2.4); });
    sprite("item_steel", s, () -> { for (int i = 0; i < 2; i++) { double y = 16 + i * 12, x = 6 + i * 4; solid(poly(x, y + 12, x + 5, y, x + 27, y, x + 32, y + 12), STEEL_L, STEEL, STEEL_D, 2.4); } seg(14, 18, 28, 18, a(Color.WHITE, 200), 2); });
    sprite("item_cloth", s, () -> { solid(rr(8, 12, 32, 26, 8), c(0xfffaf0), c(0xeedcc0), c(0xb89c78), 2.4); for (int i = 0; i < 3; i++) seg(12, 19 + i * 6, 36, 19 + i * 6, a(c(0xc2543e), 200), 2.2); solid(oval(40, 25, 5, 12), c(0xfffaf0), c(0xd8c09c), c(0xa08060), 2.2); });
    sprite("item_starglass", s, () -> { glow(24, 24, 23, STAR_L, 170); solid(star(24, 24, 5, 20, 9, -1.57), Color.WHITE, STAR, STAR_D, 2.4); fill(star(24, 24, 5, 9, 4, -1.57), a(Color.WHITE, 220)); });
    sprite("item_powder", s, () -> { solid(poly(13, 18, 35, 18, 40, 40, 8, 40), c(0xc9a878), c(0x94744e), c(0x5c4432), 2.4); solid(oval(24, 18, 11, 5), c(0x6a6474), c(0x3a3644), c(0x24222c), 2.2); Path2D f = new Path2D.Double(); f.moveTo(26, 16); f.quadTo(34, 8, 38, 12); line(f, CREAM, 2.4); fill(circle(39, 11, 3.5), FIRE_L); });
    sprite("vent", T, () -> { for (int i = 0; i < 7; i++) fill(blob(10 + r.nextInt(44), 10 + r.nextInt(44), 5 + r.nextInt(6), .3, 7, r), a(c(0x7a6a66), 150)); Shape crack = blob(32, 32, 15, .35, 9, r); fill(crack, c(0x2a1c20)); line(crack, a(c(0xb0958a), 230), 3); glow(32, 32, 12, FIRE, 130); });
  }

  static void works2() throws Exception {
    sprite("strainer", T, () -> { solid(rr(4, 4, 56, 56, 10), COPPER_L, COPPER, COPPER_D); cream(circle(32, 32, 18)); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2, x = 32 + Math.cos(an) * 25, y = 32 + Math.sin(an) * 25; fill(circle(x, y, 3.5), INK); } seg(21, 43, 43, 21, a(c(0xc0402c), 200), 4); });
    sprite("weir", T, () -> { copper(rr(4, 4, 56, 56, 10)); fill(rr(22, 8, 20, 48, 5), c(0x6b4a3a)); fill(rr(8, 24, 48, 16, 5), c(0x6b4a3a)); fill(poly(18, 24, 8, 32, 18, 40), CREAM); fill(poly(46, 24, 56, 32, 46, 40), CREAM); seg(18, 32, 46, 32, CREAM, 4); fill(poly(26, 18, 32, 10, 38, 18), c(0xf2b27a)); });
    sprite("great_store", T * 3, () -> { planks(5, 5, 182, 182, 10, true); steel(rr(20, 20, 152, 152, 12)); planks(32, 32, 128, 128, 8, false); for (int i = 0; i < 2; i++) { steel(rect(64 + i * 52, 26, 12, 140)); steel(rect(26, 64 + i * 52, 140, 12)); } brass(circle(96, 96, 20)); fill(circle(96, 96, 8), INK); bolts(13, 13, 166, 166); });
    sprite("catapult", T * 3, () -> { stonefoot(3); wood(circle(96, 96, 70)); steel(ring(96, 96, 70, 58)); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; bolt(96 + Math.cos(an) * 64, 96 + Math.sin(an) * 64); } });
    sprite("catapult_top", T * 3, () -> { wood(rr(30, 64, 120, 14, 5)); wood(rr(30, 114, 120, 14, 5)); steel(rr(40, 80, 128, 32, 8)); wood(rr(120, 72, 44, 48, 8)); fill(rr(128, 80, 28, 32, 5), c(0x3d2f30)); brass(gear(58, 96, 10, 26, 20, 8)); });
    sprite("crate", 40, () -> { planks(4, 4, 32, 32, 4, false); seg(6, 6, 34, 34, WOOD_D, 4); seg(34, 6, 6, 34, WOOD_D, 4); });
    sprite("cistern", T * 2, () -> { footing(2); solid(circle(64, 64, 49), c(0xd7ecef), c(0x8bb5bd), c(0x4c7480)); fill(circle(64, 64, 38), c(0x1e3a4c)); line(circle(64, 64, 38), INK, 3); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; bolt(64 + Math.cos(an) * 44, 64 + Math.sin(an) * 44); } });
    sprite("cistern_fill", T * 2, () -> { fill(circle(64, 64, 37), new RadialGradientPaint(52f, 50f, 50f, new float[]{0f, 1f}, new Color[]{Color.WHITE, gray(190)})); for (int i = 0; i < 3; i++) { Path2D p = new Path2D.Double(); double y = 52 + i * 13, x = 44 + r.nextInt(14); p.moveTo(x, y); p.quadTo(x + 9, y - 4, x + 18, y); line(p, a(Color.WHITE, 220), 3); } });
    sprite("culvert", T, () -> { stone(rr(6, 6, 52, 52, 12)); solid(circle(32, 32, 19), c(0xd7ecef), c(0x8bb5bd), c(0x4c7480)); fill(circle(32, 32, 12), c(0x1e3a4c)); line(circle(32, 32, 12), INK, 2.4); bolts(12, 12, 40, 40); });
    sprite("burner", T, () -> { iron(rr(4, 4, 56, 56, 10)); fill(circle(32, 32, 20), c(0x2b1a1c)); line(circle(32, 32, 20), INK, 3); glow(32, 34, 18, FIRE, 130); for (int i = 0; i < 4; i++) seg(18 + i * 9, 18, 18 + i * 9, 46, IRON_D, 3); });
    sprite("burner_hot", T, () -> { glow(32, 32, 24, FIRE, 255); fill(circle(32, 32, 9), FIRE_L); });
    sprite("bore", T * 4, () -> { footing(4); fill(circle(128, 128, 100), c(0x332c34)); line(circle(128, 128, 100), INK, 4); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + Math.PI / 4; double x = 128 + Math.cos(an) * 116, y = 128 + Math.sin(an) * 116; steel(circle(x, y, 20)); fill(circle(x, y, 8), INK); } });
    sprite("bore_top", T * 4, () -> { steel(gear(128, 128, 22, 97, 86, 0)); for (int i = 0; i < 6; i++) { Path2D p = new Path2D.Double(); p.moveTo(128, 118); p.quadTo(168, 100, 208, 122); p.lineTo(204, 142); p.quadTo(166, 128, 128, 142); p.closePath(); solid(turned(p, i * Math.PI / 3, 128, 128), BRASS_L, BRASS, BRASS_D, 3); } copper(circle(128, 128, 32)); starry(circle(128, 128, 13)); });
    sprite("garden", T * 3, () -> { wood(rr(3, 3, 186, 186, 12)); fill(rr(13, 13, 166, 166, 8), c(0x6b4a34)); for (int i = 0; i < 6; i++) { double y = 26 + i * 27; seg(20, y, 172, y, a(c(0x4a3024), 220), 8); seg(20, y - 3, 172, y - 3, a(c(0x8a6448), 160), 3); } line(rr(13, 13, 166, 166, 8), INK, 3); brass(circle(96, 96, 22)); for (int i = 0; i < 6; i++) seg(96, 96, 96 + Math.cos(i * 1.047) * 18, 96 + Math.sin(i * 1.047) * 18, INK, 3); });
    sprite("garden_crop", T * 3, () -> { for (int i = 0; i < 6; i++) for (int j = 0; j < 8; j++) { double x = 26 + j * 20 + r.nextInt(6), y = 24 + i * 27 + r.nextInt(4); if (Math.hypot(x - 96, y - 96) < 30) continue; seg(x, y + 6, x - 1, y - 8, LEAF_D, 2.6); seg(x, y, x + 5, y - 5, LEAF, 2.4); for (int p = 0; p < 5; p++) fill(circle(x - 1 + Math.cos(p * 1.2566) * 4, y - 9 + Math.sin(p * 1.2566) * 4, 2.8), c(0x8fb4f0)); fill(circle(x - 1, y - 9, 2), c(0xfff1b8)); } });
    sprite("steelworks", T * 3, () -> { brick(4, 4, 184, 184, c(0x7c6a70)); steel(rr(22, 22, 148, 60, 12)); furnace(34, 100, 124, 70); for (int i = 0; i < 4; i++) bolt(40 + i * 37, 34); steel(circle(52, 56, 16)); steel(circle(140, 56, 16)); fill(circle(52, 56, 8), INK); fill(circle(140, 56, 8), INK); });
    sprite("steelworks_hot", T * 3, () -> { glow(96, 136, 60, FIRE, 200); for (int i = 0; i < 4; i++) fill(rr(48 + i * 26, 112, 16, 46, 5), a(c(0xfff0b0), 190)); });
    sprite("powder_mill", T * 2, () -> { footing(2); solid(circle(64, 64, 47), STONE_L, STONE, STONE_D); fill(circle(64, 64, 34), c(0x4a4450)); line(circle(64, 64, 34), INK, 3); wood(rr(2, 52, 18, 24, 5)); wood(rr(108, 52, 18, 24, 5)); });
    sprite("powder_mill_top", T * 2, () -> { for (int i = 0; i < 2; i++) { stone(turned(rr(70, 52, 22, 24, 6), i * Math.PI, 64, 64)); wood(turned(rr(60, 60, 34, 8, 3), i * Math.PI, 64, 64)); } brass(circle(64, 64, 10)); });
    sprite("star_furnace", T * 3, () -> { brick(4, 4, 184, 184, c(0x4c4668)); starry(ring(96, 96, 76, 60)); for (int i = 0; i < 12; i++) { double an = i * Math.PI / 6; fill(star(96 + Math.cos(an) * 68, 96 + Math.sin(an) * 68, 4, 7, 3, an), Color.WHITE); } steel(ring(96, 96, 60, 50)); fill(circle(96, 96, 50), c(0x1c1428)); line(circle(96, 96, 50), INK, 3.4); });
    sprite("star_furnace_hot", T * 3, () -> { fill(circle(96, 96, 48), new RadialGradientPaint(96f, 96f, 48f, new float[]{0f, .5f, 1f}, new Color[]{Color.WHITE, c(0xc9a8ff), c(0x4a2a90)})); fill(star(96, 96, 8, 44, 14, 0), a(Color.WHITE, 170)); });
    sprite("press", T * 2, () -> { footing(2); wood(rr(18, 18, 92, 92, 12)); solid(circle(64, 64, 36), c(0xffe08a), c(0xc88a2e), c(0x7a5020)); fill(circle(64, 64, 26), c(0x5a3a1a)); line(circle(64, 64, 26), INK, 3); });
    sprite("press_top", T * 2, () -> { iron(circle(64, 64, 22)); wood(rr(20, 58, 88, 12, 5)); wood(rr(58, 20, 12, 88, 5)); brass(circle(64, 64, 10)); });
    sprite("chiller", T * 2, () -> { footing(2); solid(rr(16, 16, 96, 96, 14), c(0xf4fdff), c(0xa8dce8), c(0x5a94ac)); for (int i = 0; i < 5; i++) steel(rr(24 + i * 17, 22, 10, 84, 4)); fill(circle(64, 64, 24), c(0x1e3a5c)); line(circle(64, 64, 24), INK, 3); });
    sprite("chiller_top", T * 2, () -> { for (int i = 0; i < 6; i++) solid(turned(poly(64, 60, 84, 54, 86, 64, 64, 68), i * Math.PI / 3, 64, 64), Color.WHITE, c(0xc8ecf8), c(0x7ab0cc), 2); steel(circle(64, 64, 7)); });
    sprite("boiler", T * 3, () -> { stonefoot(3); copper(circle(96, 96, 78)); for (int i = 0; i < 3; i++) line(circle(96, 96, 72 - i * 18), a(INK, 110), 3); furnace(56, 110, 80, 52); steel(circle(96, 66, 24)); fill(circle(96, 66, 14), c(0x241820)); for (int i = 0; i < 10; i++) { double an = i * .628; bolt(96 + Math.cos(an) * 70, 96 + Math.sin(an) * 70); } });
    sprite("boiler_hot", T * 3, () -> { glow(96, 138, 46, FIRE, 220); });
    sprite("vent_well", T * 2, () -> { stone(circle(64, 64, 56)); steel(ring(64, 64, 46, 30)); fill(circle(64, 64, 30), c(0x2a1c20)); line(circle(64, 64, 30), INK, 3); glow(64, 64, 26, FIRE, 150); });
    sprite("vent_well_top", T * 2, () -> { for (int i = 0; i < 4; i++) brass(turned(poly(64, 58, 92, 48, 96, 62, 64, 70), i * Math.PI / 2, 64, 64)); copper(circle(64, 64, 10)); });
    sprite("sun_court", T * 3, () -> { footing(3); for (int j = 0; j < 3; j++) for (int i = 0; i < 3; i++) { double x = 42 + i * 54, y = 42 + j * 54; copper(circle(x, y, 25)); fill(circle(x, y, 20), new RadialGradientPaint((float) x - 5, (float) y - 6, 28f, new float[]{0f, .6f, 1f}, new Color[]{c(0xf6fffb), c(0x9fe0d8), c(0x3f8f96)})); line(circle(x, y, 20), INK, 2.2); fill(turned(oval(x - 7, y - 8, 7, 4), -.7, x - 7, y - 8), a(Color.WHITE, 190)); } });
    sprite("reactor", T * 4, () -> { steel(rr(4, 4, 248, 248, 26)); fill(circle(128, 128, 104), c(0x2c2438)); line(circle(128, 128, 104), INK, 4); starry(ring(128, 128, 104, 88)); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; double x = 128 + Math.cos(an) * 68, y = 128 + Math.sin(an) * 68; steel(circle(x, y, 17)); fill(circle(x, y, 10), c(0x1a1420)); } brass(circle(128, 128, 38)); fill(circle(128, 128, 26), c(0x1a1420)); line(circle(128, 128, 26), INK, 3); for (double[] p : new double[][]{{26, 26}, {230, 26}, {26, 230}, {230, 230}}) { solid(circle(p[0], p[1], 15), c(0xf4fdff), c(0xa8dce8), c(0x5a94ac)); } });
    sprite("reactor_hot", T * 4, () -> { for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4; double x = 128 + Math.cos(an) * 68, y = 128 + Math.sin(an) * 68; glow(x, y, 26, FIRE, 220); fill(circle(x, y, 8), c(0xfff0b0)); } glow(128, 128, 60, c(0xff6a3a), 230); fill(circle(128, 128, 20), c(0xfff6d0)); });
    sprite("ratchet", T, () -> { steel(rr(6, 6, 52, 52, 10)); brass(gear(28, 32, 9, 17, 12, 5)); fill(poly(40, 20, 56, 32, 40, 44), CREAM); line(poly(40, 20, 56, 32, 40, 44), INK, 2.4); });
    sprite("great_flywheel", T * 3, () -> { footing(3); fill(circle(96, 96, 76), c(0x332c38)); line(circle(96, 96, 76), INK, 3.4); steel(ring(96, 96, 82, 75)); });
    sprite("great_flywheel_top", T * 3, () -> { steel(ring(96, 96, 72, 50)); for (int i = 0; i < 8; i++) brass(turned(rect(90, 46, 12, 50), i * Math.PI / 4, 96, 96)); copper(circle(96, 96, 22)); bolt(96, 96); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4 + .39; fill(circle(96 + Math.cos(an) * 61, 96 + Math.sin(an) * 61, 5), STAR_L); } });
    sprite("switch", T, () -> { wood(rr(8, 8, 48, 48, 10)); fill(rr(18, 24, 28, 16, 8), c(0x2c2023)); line(rr(18, 24, 28, 16, 8), INK, 2.4); });
    sprite("switch_on", T, () -> { glow(39, 32, 14, c(0x9ce07a), 200); solid(circle(39, 32, 8), c(0xe0ffd0), c(0x7ac04a), c(0x3a7a2a)); });
    sprite("switch_off", T, () -> solid(circle(25, 32, 8), c(0xffd0c0), c(0xd0503a), c(0x7a2a24)));
    sprite("governor", T, () -> { teal(rr(6, 6, 52, 52, 10)); brass(circle(32, 32, 20)); cream(circle(32, 32, 13)); seg(32, 32, 40, 24, INK, 3); fill(circle(32, 32, 3), INK); });
    sprite("wagon_post", T * 3, () -> { planks(5, 5, 182, 182, 9, false); solid(rr(40, 30, 112, 96, 12), c(0xfffaf0), c(0xe8d4b0), c(0xb09870)); for (int i = 0; i < 3; i++) { Path2D q = new Path2D.Double(); q.moveTo(44, 50 + i * 26); q.quadTo(96, 40 + i * 26, 148, 50 + i * 26); line(q, a(c(0x9c6a40), 150), 3); } for (double[] w : new double[][]{{48, 140}, {144, 140}, {48, 28}, {144, 28}}) { wood(circle(w[0], w[1], 18)); for (int i = 0; i < 6; i++) seg(w[0], w[1], w[0] + Math.cos(i * 1.047) * 14, w[1] + Math.sin(i * 1.047) * 14, WOOD_D, 3); brass(circle(w[0], w[1], 5)); } wood(rr(88, 130, 16, 56, 5)); });
    for (int k = 0; k < 2; k++) { final int n = k; final int size = 4 + k; final double m = size * T / 2.0; sprite(k == 0 ? "great_hearth" : "grand_hearth", size * T, () -> { stonefoot(size); if (n == 1) { for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2 + Math.PI / 4; starry(circle(m + Math.cos(an) * 190, m + Math.sin(an) * 190, 22)); } steel(circle(m, m, m - 18)); } else copper(circle(m, m, m - 16)); brick(m - 64, m - 64, 128, 128, c(0xb9624a)); brass(ring(m, m, 70, 56)); fill(circle(m, m, 56), c(0x2b1a1c)); line(circle(m, m, 56), INK, 4); for (int i = 0; i < 6; i++) wood(turned(rr(m - 44, m - 8, 88, 16, 7), i * .52, m, m)); for (int i = 0; i < 16; i++) { double an = i * Math.PI / 8; bolt(m + Math.cos(an) * (m - 30), m + Math.sin(an) * (m - 30)); } }); }
    sprite("refit", T * 3, () -> { footing(3); planks(16, 16, 160, 160, 8, true); teal(ring(96, 96, 74, 56)); fill(circle(96, 96, 56), c(0x2d3a44)); line(circle(96, 96, 56), INK, 3); for (int i = 0; i < 4; i++) { double an = i * Math.PI / 2; brass(turned(rr(140, 88, 36, 16, 5), an, 96, 96)); } brass(rr(84, 160, 24, 30, 5)); });
    sprite("star_forge", T * 5, () -> { brick(4, 4, 312, 312, c(0x4c4668)); starry(ring(160, 160, 138, 116)); steel(ring(160, 160, 116, 98)); fill(circle(160, 160, 98), c(0x1c1428)); line(circle(160, 160, 98), INK, 4); for (int i = 0; i < 8; i++) { double an = i * Math.PI / 4 + .39; fill(star(160 + Math.cos(an) * 127, 160 + Math.sin(an) * 127, 5, 13, 5, an), Color.WHITE); } for (double[] q : new double[][]{{36, 36}, {284, 36}, {36, 284}, {284, 284}}) { steel(circle(q[0], q[1], 24)); fill(circle(q[0], q[1], 12), c(0x241820)); } steel(rr(130, 262, 60, 54, 8)); fill(poly(144, 276, 176, 276, 160, 306), STAR_L); });
    sprite("steel_wall", T, () -> { steel(rr(3, 3, 58, 58, 8)); steel(rr(12, 12, 40, 40, 6)); seg(14, 14, 50, 50, a(INK, 90), 3); seg(50, 14, 14, 50, a(INK, 90), 3); bolts(8, 8, 48, 48); });
    sprite("steel_wall_large", T * 2, () -> { steel(rr(3, 3, 122, 122, 12)); steel(rr(16, 16, 96, 96, 9)); for (int i = 0; i < 2; i++) { seg(20, 20 + i * 88, 108, 108 - i * 88, a(INK, 90), 5); } steel(circle(64, 64, 22)); brass(circle(64, 64, 10)); bolts(10, 10, 108, 108); });
    sprite("star_wall", T, () -> { starry(rr(3, 3, 58, 58, 10)); fill(star(32, 32, 4, 22, 8, .78), a(Color.WHITE, 190)); line(rr(10, 10, 44, 44, 6), a(Color.WHITE, 130), 2); });
    sprite("gate", T, () -> { planks(3, 3, 58, 58, 5, true); copper(rect(3, 12, 58, 8)); copper(rect(3, 44, 58, 8)); brass(circle(32, 32, 7)); });
    sprite("gate_large", T * 2, () -> { planks(3, 3, 122, 122, 8, true); copper(rect(3, 22, 122, 13)); copper(rect(3, 93, 122, 13)); seg(64, 6, 64, 122, INK, 4); brass(circle(50, 64, 9)); brass(circle(78, 64, 9)); });
    sprite("caltrop", T, () -> { for (int k = 0; k < 4; k++) { double x = 18 + (k % 2) * 28, y = 18 + (k / 2) * 28; for (int i = 0; i < 4; i++) iron(turned(poly(x, y - 3, x + 13, y, x, y + 3), i * Math.PI / 2 + k * .5, x, y)); } });
    sprite("aegis", T * 3, () -> { stonefoot(3); steel(circle(96, 96, 76)); for (int i = 0; i < 6; i++) { double an = i * Math.PI / 3; solid(circle(96 + Math.cos(an) * 58, 96 + Math.sin(an) * 58, 13), c(0xe8ffff), c(0x8ad8f0), c(0x3a7aa8)); } fill(circle(96, 96, 34), c(0x1e2a44)); line(circle(96, 96, 34), INK, 3); });
    sprite("aegis_top", T * 3, () -> { Path2D h = new Path2D.Double(); for (int i = 0; i < 6; i++) { double an = i * Math.PI / 3; if (i == 0) h.moveTo(96 + Math.cos(an) * 28, 96 + Math.sin(an) * 28); else h.lineTo(96 + Math.cos(an) * 28, 96 + Math.sin(an) * 28); } h.closePath(); solid(h, Color.WHITE, c(0x9fe8ff), c(0x3a8ac8)); fill(circle(90, 90, 8), a(Color.WHITE, 180)); });
    sprite("metronome", T * 2, () -> { footing(2); wood(poly(30, 110, 46, 20, 82, 20, 98, 110)); cream(poly(40, 102, 52, 30, 76, 30, 88, 102)); for (int i = 0; i < 5; i++) seg(58, 40 + i * 12, 70, 40 + i * 12, a(INK, 130), 2); });
    sprite("metronome_top", T * 2, () -> { brass(rr(61, 24, 6, 78, 3)); starry(rr(52, 40, 24, 16, 5)); copper(circle(64, 100, 8)); });
    sprite("tinker_hall", T * 2, () -> { wood(rr(4, 4, 120, 120, 16)); steel(rr(14, 14, 100, 100, 12)); cream(circle(64, 64, 42)); fill(rr(56, 30, 16, 68, 4), c(0xd1503e)); fill(rr(30, 56, 68, 16, 4), c(0xd1503e)); bolts(22, 22, 84, 84); });
    sprite("hedge", T, () -> { for (int i = 0; i < 8; i++) { Shape th = turned(poly(32, 27, 62, 32, 32, 37), i * .785, 32, 32); solid(th, c(0xc9b27a), c(0x8f7048), c(0x5c4030), 2); } solid(blob(32, 32, 22, .16, 10, r), LEAF_L, LEAF_D, c(0x24402a), 3); for (int i = 0; i < 5; i++) fill(blob(20 + r.nextInt(24), 20 + r.nextInt(24), 5, .3, 7, r), a(LEAF, 200)); fill(circle(27, 28, 3), THORN); fill(circle(38, 36, 3), THORN); });
    // Turret heads.
    sprite("squib_top", T, () -> { for (int i = -1; i <= 1; i++) { solid(rr(26, 27 + i * 12, 32, 10, 4), c(0xffd0e8), new Color[]{c(0xe86a8a), c(0x5aa8d8), c(0xe8b04a)}[i + 1], c(0x7a3a4a), 2.4); } wood(circle(26, 32, 15)); brass(circle(26, 32, 6)); });
    sprite("bellows_top", T * 2, () -> { solid(poly(16, 34, 66, 50, 66, 78, 16, 94), c(0xd8a878), c(0x9c6a44), c(0x5c3c2a)); for (int i = 0; i < 3; i++) seg(26 + i * 12, 38 + i * 4, 26 + i * 12, 90 - i * 4, a(INK, 150), 3); iron(rr(62, 50, 46, 28, 8)); copper(rr(98, 44, 18, 40, 7)); glow(114, 64, 14, FIRE, 230); });
    sprite("skyrack_top", T * 3, () -> { wood(rr(44, 40, 96, 112, 14)); for (int j = 0; j < 4; j++) { steel(rr(62, 48 + j * 25, 92, 19, 6)); fill(circle(150, 57.5 + j * 25, 7), INK); solid(poly(150, 51 + j * 25, 166, 57.5 + j * 25, 150, 64 + j * 25), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2.2); } copper(circle(58, 96, 24)); brass(circle(58, 96, 11)); });
    sprite("longbow_top", T * 3, () -> { Path2D bow = new Path2D.Double(); bow.moveTo(112, 6); bow.curveTo(160, 50, 160, 142, 112, 186); line(bow, INK, 20); line(bow, STEEL, 13); line(bow, STEEL_L, 4); seg(112, 8, 58, 96, c(0xf7ecd0), 3); seg(112, 184, 58, 96, c(0xf7ecd0), 3); steel(rr(20, 78, 156, 36, 10)); starry(rr(60, 88, 110, 16, 5)); copper(circle(52, 96, 24)); brass(circle(52, 96, 10)); });
    sprite("mortar_row_top", T * 3, () -> { wood(rr(34, 30, 70, 132, 14)); for (int j = 0; j < 4; j++) { iron(rr(84, 34 + j * 32, 76, 26, 9)); copper(rr(148, 32 + j * 32, 14, 30, 5)); fill(oval(158, 47 + j * 32, 4, 9), INK); } brass(circle(66, 96, 24)); bolt(66, 96); });
    sprite("grapnel_top", T * 2, () -> { wood(circle(58, 64, 34)); brass(gear(58, 64, 10, 26, 20, 9)); steel(rr(74, 56, 34, 16, 5)); Path2D h = new Path2D.Double(); h.moveTo(106, 64); h.quadTo(126, 64, 118, 44); line(h, INK, 9); line(h, STEEL, 5); Path2D q = new Path2D.Double(); q.moveTo(106, 64); q.quadTo(126, 64, 118, 84); line(q, INK, 9); line(q, STEEL, 5); });
    sprite("swatter_top", T * 2, () -> { teal(circle(64, 64, 36)); steel(circle(64, 64, 24)); for (int i = 0; i < 3; i++) starry(circle(64 + Math.cos(i * 2.094) * 24, 64 + Math.sin(i * 2.094) * 24, 8)); solid(circle(64, 64, 11), Color.WHITE, c(0xffe6a0), c(0xd09a3a)); });
  }

  static void creatures2() throws Exception {
    for (int k = 0; k < 2; k++) { final int n = k; sprite(k == 0 ? "u_lantern_great" : "u_lantern_grand", 76, () -> { if (n == 1) glow(38, 38, 36, FIRE_L, 140); solid(oval(38, 38, 24, 26), c(0xfff6d8), n == 0 ? c(0xffb85a) : c(0xffd27a), c(0xc46a34)); for (int i = -1; i <= 1; i++) { Path2D p = new Path2D.Double(); p.moveTo(38 + i * 12, 13 + Math.abs(i) * 3); p.quadTo(38 + i * 18, 38, 38 + i * 12, 63 - Math.abs(i) * 3); line(p, a(c(0x8c3a24), 170), 2.4); } if (n == 0) { brass(rr(25, 5, 26, 11, 5)); brass(rr(25, 60, 26, 11, 5)); } else { starry(rr(25, 5, 26, 11, 5)); starry(rr(25, 60, 26, 11, 5)); } brass(ring(38, 38, 13, 9)); fill(circle(45, 31, 3.4), INK); fill(circle(45, 45, 3.4), INK); fill(circle(46, 30, 1.2), Color.WHITE); fill(circle(46, 44, 1.2), Color.WHITE); }); }
    sprite("u_stag", 84, () -> { steel(oval(36, 42, 27, 23)); seg(9, 42, 60, 42, INK, 3); copper(circle(60, 42, 14)); for (int k = -1; k <= 1; k += 2) { Path2D h = new Path2D.Double(); h.moveTo(66, 42 + k * 8); h.quadTo(86, 42 + k * 20, 74, 42 + k * 4); line(h, INK, 8); line(h, BRASS, 4.4); } eyes(64, 42, 6, 3); for (int i = 0; i < 3; i++) { bolt(22 + i * 11, 30); bolt(22 + i * 11, 54); } brass(rr(28, 36, 22, 12, 4)); });
    sprite("u_colossus", 150, () -> { steel(rr(18, 12, 60, 34, 12)); steel(rr(18, 104, 60, 34, 12)); steel(rr(20, 24, 90, 102, 24)); starry(circle(62, 75, 30)); fill(circle(62, 75, 18), c(0x1c1428)); glow(62, 75, 26, STAR_L, 240); fill(star(62, 75, 6, 16, 6, 0), Color.WHITE); brass(circle(114, 75, 24)); eyes(122, 75, 9, 5); iron(rr(78, 8, 54, 22, 8)); iron(rr(78, 120, 54, 22, 8)); fill(circle(130, 19, 6), INK); fill(circle(130, 131, 6), INK); });
    sprite("u_heron", 100, () -> { solid(oval(46, 50, 34, 16), c(0xfffaf0), c(0xe0e8f0), c(0x8aa0b8)); copper(oval(38, 50, 18, 10)); for (int i = 0; i < 2; i++) { iron(circle(30 + i * 16, 50, 5)); } solid(circle(80, 50, 12), c(0xfffaf0), c(0xe0e8f0), c(0x8aa0b8)); solid(poly(88, 46, 100, 50, 88, 54), FIRE_L, c(0xe8923a), c(0x9c5a24), 2.2); eyes(84, 50, 6, 2.8); });
    sprite("wing_sail", 96, 60, () -> { Path2D p = new Path2D.Double(); p.moveTo(8, 54); p.curveTo(20, 10, 60, -2, 92, 10); p.curveTo(84, 22, 74, 26, 72, 40); p.curveTo(56, 34, 46, 48, 40, 54); p.closePath(); solid(p, c(0xfffaf0), c(0xe8dcc8), c(0xb09870), 2.6); for (int i = 0; i < 3; i++) seg(12, 52, 80 - i * 18, 14 + i * 12, a(c(0x9c6a40), 150), 2); });
    sprite("u_skybarge", 180, () -> { solid(oval(90, 90, 80, 52), c(0xfff6e0), c(0xe86a52), c(0x7a2a2a)); for (int i = -2; i <= 2; i++) { Path2D p = new Path2D.Double(); p.moveTo(12, 90); p.quadTo(90, 90 + i * 46, 168, 90); line(p, a(c(0xfff0d2), 190), 3); } wood(rr(50, 70, 84, 40, 14)); for (int i = 0; i < 3; i++) { steel(circle(66 + i * 24, 78, 7)); steel(circle(66 + i * 24, 102, 7)); } brass(circle(138, 90, 15)); eyes(143, 90, 6, 3.4); starry(circle(60, 90, 10)); });
    sprite("u_wren", 56, () -> { solid(oval(24, 28, 16, 12), c(0xd8a878), c(0x9c6a44), c(0x5c3c2a)); solid(circle(40, 28, 10), c(0xe8c090), c(0xb07c50), c(0x6a4630)); iron(poly(47, 24, 56, 28, 47, 32)); eyes(43, 28, 4, 2.2); brass(circle(22, 28, 6)); });
    sprite("u_mason", 68, () -> { teal(oval(30, 34, 22, 16)); brass(rr(18, 26, 22, 16, 5)); solid(circle(50, 34, 12), TEAL_L, TEAL, TEAL_D); eyes(54, 34, 5, 2.6); steel(rr(54, 20, 12, 5, 2)); steel(rr(54, 43, 12, 5, 2)); });
    sprite("u_tender", 92, () -> { cream(oval(42, 46, 32, 24)); fill(rr(36, 32, 10, 28, 3), c(0xd1503e)); fill(rr(27, 41, 28, 10, 3), c(0xd1503e)); solid(circle(70, 46, 15), c(0xfff6e0), CREAM, c(0xc9b08c)); eyes(75, 46, 6, 3.2); });
    sprite("u_frostling", T, () -> { for (int i = 0; i < 6; i++) { Shape th = turned(poly(32, 26, 60, 32, 32, 38), i * 1.047, 32, 32); solid(th, Color.WHITE, c(0xbfe4fa), c(0x5a9ac8), 2); } solid(circle(33, 32, 15), c(0xf4fbff), c(0xa8d4ee), c(0x4f88b4)); fill(circle(40, 27, 4), INK); fill(circle(40, 37, 4), INK); fill(circle(41, 26, 1.8), c(0x8ff0ff)); fill(circle(41, 36, 1.8), c(0x8ff0ff)); });
    sprite("u_catkin", 72, () -> { for (int i = 0; i < 5; i++) solid(circle(18 + i * 8, 36, 13 - i * 1.4), c(0xf4e8a0), c(0xc9b04a), c(0x7a6a2c), 2.2); solid(rr(40, 26, 28, 20, 8), BARK_L, BARK, BARK_D); fill(oval(66, 36, 4, 7), INK); eyes(44, 36, 7, 3); });
    sprite("u_dewmoth", 72, () -> { solid(oval(32, 36, 18, 9), c(0xe8fff8), c(0x8ad8c8), c(0x3a8a84)); solid(circle(50, 36, 10), c(0xe8fff8), c(0x8ad8c8), c(0x3a8a84)); eyes(54, 36, 5, 2.8); glow(28, 36, 12, c(0xa8ffe0), 190); });
    sprite("wing_dew", 64, 48, () -> { Path2D p = new Path2D.Double(); p.moveTo(6, 42); p.curveTo(6, 8, 40, -6, 60, 12); p.curveTo(64, 30, 36, 46, 6, 42); solid(p, c(0xf0fffa), c(0xa8e8dc), c(0x4a9a94), 2.2); fill(circle(38, 18, 7), a(Color.WHITE, 200)); fill(circle(38, 18, 3.4), c(0x3a8a84)); });
    sprite("u_stormseed", 110, () -> { for (int i = 0; i < 22; i++) { double an = i * .2856; seg(55, 55, 55 + Math.cos(an) * 46, 55 + Math.sin(an) * 46, a(c(0xe8e0ff), 190), 2); fill(circle(55 + Math.cos(an) * 47, 55 + Math.sin(an) * 47, 5), a(c(0xd8c8f0), 230)); } solid(circle(55, 55, 24), c(0xa890c8), c(0x5c4480), c(0x2c2048), 3.4); eyes(64, 55, 9, 5); });
    sprite("u_briarhulk", 124, () -> { for (int i = 0; i < 12; i++) { Shape th = turned(poly(62, 52, 120, 62, 62, 72), i * .5236, 62, 62); solid(th, c(0xc9b27a), c(0x8f7048), c(0x5c4030), 2.4); } solid(blob(62, 62, 44, .1, 12, r), LEAF_L, LEAF_D, c(0x1e3424), 4); for (int i = 0; i < 12; i++) fill(blob(34 + r.nextInt(54), 34 + r.nextInt(54), 7, .3, 8, r), a(LEAF, 190)); for (int i = 0; i < 4; i++) { double x = 42 + r.nextInt(34), y = 40 + r.nextInt(44); solid(circle(x, y, 5), c(0xd88aa0), THORN, THORN_D, 2); } eyes(88, 62, 12, 6); solid(poly(98, 52, 118, 62, 98, 72), c(0x2b1a1c), c(0x2b1a1c), c(0x2b1a1c), 2); });
    sprite("u_queen", 160, () -> { solid(oval(58, 80, 46, 28), c(0xffd27a), c(0xd98a2e), c(0x7a4018), 4); for (int i = 0; i < 5; i++) seg(28 + i * 14, 56 + Math.abs(i - 2) * 2, 28 + i * 14, 104 - Math.abs(i - 2) * 2, INK, 5); solid(circle(112, 80, 26), c(0xd88aa0), THORN, THORN_D, 4); for (int i = 0; i < 5; i++) { double an = -.9 + i * .45; solid(turned(poly(100, 74, 148, 80, 100, 86), an, 112, 80), BRASS_L, BRASS, BRASS_D, 2); } solid(circle(112, 80, 20), c(0xe8a0b4), c(0x8f5078), THORN_D, 3); eyes(120, 80, 9, 5.4); });
    sprite("shot_squib", 32, () -> { glow(16, 16, 14, c(0xffb0e0), 190); solid(rr(6, 11, 19, 10, 5), c(0xffe0f0), c(0xe86a8a), c(0x8c2a4a), 2); });
    sprite("shot_flame", 48, () -> { glow(24, 24, 23, FIRE, 230); fill(blob(24, 24, 11, .3, 8, r), a(c(0xfff0b0), 220)); });
    sprite("shot_rocket", 56, 24, () -> { solid(rr(10, 6, 32, 12, 5), c(0xfff6e0), c(0xe0c8a8), c(0x94785c), 2.2); solid(poly(40, 5, 54, 12, 40, 19), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2.2); solid(poly(4, 2, 16, 9, 16, 15, 4, 22), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2); });
  }

  static void effects2() throws Exception {
    for (int v = 0; v < 2; v++) sprite("scorch" + v, 96, () -> { for (int i = 0; i < 14; i++) { double an = r.nextDouble() * 6.28, d = r.nextDouble() * 26; fill(circle(48 + Math.cos(an) * d, 48 + Math.sin(an) * d, 8 + r.nextInt(12)), a(c(0x1c1416), 34)); } fill(circle(48, 48, 20), new RadialGradientPaint(48f, 48f, 20f, new float[]{0f, 1f}, new Color[]{a(c(0x140e10), 150), a(c(0x140e10), 0)})); for (int i = 0; i < 9; i++) { double an = r.nextDouble() * 6.28; seg(48 + Math.cos(an) * 14, 48 + Math.sin(an) * 14, 48 + Math.cos(an) * (28 + r.nextInt(14)), 48 + Math.sin(an) * (28 + r.nextInt(14)), a(c(0x1c1416), 70), 3); } });
    sprite("splat", 64, () -> { for (int i = 0; i < 7; i++) { double an = r.nextDouble() * 6.28, d = r.nextDouble() * 18; fill(blob(32 + Math.cos(an) * d, 32 + Math.sin(an) * d, 4 + r.nextInt(7), .3, 7, r), a(Color.WHITE, 150)); } });
    sprite("cloud", 256, () -> { for (int i = 0; i < 26; i++) { double an = r.nextDouble() * 6.28, d = r.nextDouble() * 70; double x = 128 + Math.cos(an) * d, y = 128 + Math.sin(an) * d * .6; float rad = 34 + r.nextInt(26); fill(circle(x, y, rad), new RadialGradientPaint((float) x, (float) y, rad, new float[]{0f, 1f}, new Color[]{a(Color.BLACK, 60), a(Color.BLACK, 0)})); } });
    sprite("dome", 256, () -> { Path2D h = new Path2D.Double(); for (int i = 0; i < 6; i++) { double an = i * Math.PI / 3; if (i == 0) h.moveTo(128 + Math.cos(an) * 122, 128 + Math.sin(an) * 122); else h.lineTo(128 + Math.cos(an) * 122, 128 + Math.sin(an) * 122); } h.closePath(); fill(h, new RadialGradientPaint(128f, 128f, 124f, new float[]{0f, .7f, 1f}, new Color[]{a(Color.WHITE, 14), a(Color.WHITE, 40), a(Color.WHITE, 150)})); line(h, a(Color.WHITE, 230), 5); Shape clip = g.getClip(); g.clip(h); for (int j = -5; j < 6; j++) for (int i = -5; i < 6; i++) { double x = 128 + i * 34 + (j & 1) * 17, y = 128 + j * 30; Path2D q = new Path2D.Double(); for (int k = 0; k < 6; k++) { double an = k * Math.PI / 3 + Math.PI / 6; if (k == 0) q.moveTo(x + Math.cos(an) * 19, y + Math.sin(an) * 19); else q.lineTo(x + Math.cos(an) * 19, y + Math.sin(an) * 19); } q.closePath(); line(q, a(Color.WHITE, 46), 2); } g.setClip(clip); });
    sprite("crown", 32, () -> solid(poly(4, 26, 4, 8, 11, 16, 16, 5, 21, 16, 28, 8, 28, 26), BRASS_L, BRASS, BRASS_D, 2.4));
    icon("map", () -> { cream(poly(5, 10, 17, 5, 31, 10, 43, 5, 43, 38, 31, 43, 17, 38, 5, 43)); seg(17, 6, 17, 37, a(INK, 130), 2); seg(31, 11, 31, 42, a(INK, 130), 2); seg(10, 30, 24, 18, c(0xc0402c), 3); fill(circle(35, 22, 4), c(0xc0402c)); });
    icon("codex", () -> { wood(rr(8, 5, 32, 38, 5)); cream(rr(13, 9, 23, 30, 3)); for (int i = 0; i < 4; i++) seg(17, 16 + i * 6, 32, 16 + i * 6, a(INK, 130), 2); });
    icon("ledger", () -> { cream(rr(6, 6, 36, 36, 5)); int[] h = {12, 20, 15, 26}; for (int i = 0; i < 4; i++) solid(rr(11 + i * 7.4, 38 - h[i], 5.5, h[i], 1.5), COPPER_L, COPPER, COPPER_D, 1.6); });
    icon("layouts", () -> { for (int i = 0; i < 4; i++) copper(rr(6 + (i % 2) * 19, 6 + (i / 2) * 19, 16, 16, 4)); });
    icon("flip", () -> { fill(poly(6, 38, 20, 38, 20, 8), CREAM); line(poly(6, 38, 20, 38, 20, 8), INK, 2.6); fill(poly(42, 38, 28, 38, 28, 8), c(0xc9b08c)); line(poly(42, 38, 28, 38, 28, 8), INK, 2.6); });
    icon("hand", () -> { solid(poly(16, 44, 10, 28, 15, 25, 18, 30, 18, 8, 23, 8, 24, 22, 27, 22, 28, 12, 33, 13, 33, 26, 38, 22, 41, 26, 34, 44), c(0xfff6e0), CREAM, c(0xc9b08c), 2.6); });
    icon("sword", () -> { seg(12, 36, 36, 10, INK, 9); seg(12, 36, 36, 10, STEEL_L, 5); seg(10, 26, 22, 38, INK, 8); seg(10, 26, 22, 38, BRASS, 4); });
    icon("shield", () -> { Path2D p = new Path2D.Double(); p.moveTo(24, 4); p.lineTo(42, 10); p.curveTo(42, 28, 36, 38, 24, 45); p.curveTo(12, 38, 6, 28, 6, 10); p.closePath(); steel(p); });
    icon("loop", () -> { line(new Arc2D.Double(8, 14, 32, 20, 30, 300, Arc2D.OPEN), INK, 9); line(new Arc2D.Double(8, 14, 32, 20, 30, 300, Arc2D.OPEN), CREAM, 5); cream(poly(32, 8, 46, 16, 32, 24)); });
    icon("pen", () -> { solid(poly(8, 40, 12, 28, 34, 6, 42, 14, 20, 36), BRASS_L, BRASS, BRASS_D, 2.6); fill(poly(8, 40, 12, 28, 20, 36), CREAM); });
    icon("save", () -> { teal(rr(6, 6, 36, 36, 5)); cream(rr(13, 8, 22, 14, 2)); cream(rr(12, 26, 24, 14, 2)); });
    icon("wagon", () -> { cream(rr(8, 8, 32, 22, 9)); wood(rr(5, 26, 38, 8, 3)); wood(circle(14, 37, 7)); wood(circle(34, 37, 7)); });
    icon("search", () -> { line(circle(20, 20, 12), INK, 9); line(circle(20, 20, 12), CREAM, 5); seg(29, 29, 41, 41, INK, 10); seg(29, 29, 41, 41, CREAM, 5); });
    icon("deed", () -> { brass(circle(24, 20, 15)); fill(star(24, 20, 5, 10, 4, -1.57), CREAM); solid(poly(14, 30, 24, 34, 20, 46), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2); solid(poly(34, 30, 24, 34, 28, 46), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2); });
    icon("back", () -> cream(poly(8, 24, 26, 8, 26, 17, 41, 17, 41, 31, 26, 31, 26, 40)));
    icon("gear", () -> { steel(gear(24, 24, 8, 20, 15, 7)); });
    icon("brush", () -> { seg(14, 34, 38, 8, INK, 9); seg(14, 34, 38, 8, WOOD_L, 5); solid(blob(13, 36, 8, .2, 7, r), c(0xffd0c0), c(0xd0503a), c(0x7a2a24), 2); });
    icon("pin", () -> { solid(poly(24, 45, 12, 22, 36, 22), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2.4); solid(circle(24, 18, 13), c(0xffb0a0), c(0xd0503a), c(0x7a2a24), 2.6); fill(circle(24, 18, 5), CREAM); });
  }

  // As generated assets are named.
  static String camel(String n) {
    StringBuilder b = new StringBuilder();
    for (String part : n.split("_")) b.append(b.length() == 0 ? part : Character.toUpperCase(part.charAt(0)) + part.substring(1));
    return b.toString();
  }

  static void third() throws Exception {
    // On the water.
    sprite("slipway", T * 3, () -> { planks(6, 6, 180, 180, 9, false); line(rr(4, 4, 184, 184, 10), INK, 4); fill(rr(52, 30, 88, 150, 10), c(0x2f5f78)); line(rr(52, 30, 88, 150, 10), INK, 3.4); for (int i = 0; i < 5; i++) wood(rr(46, 44 + i * 28, 100, 9, 4)); wood(rr(40, 8, 12, 176, 5)); wood(rr(140, 8, 12, 176, 5)); brass(gear(24, 24, 8, 17, 12, 5)); copper(circle(168, 24, 15)); for (int i = 0; i < 4; i++) bolt(46, 24 + i * 46); });
    sprite("drydock", T * 4, () -> { stonefoot(4); fill(rr(56, 22, 144, 214, 16), c(0x2a4f66)); line(rr(56, 22, 144, 214, 16), INK, 4); for (int i = 0; i < 6; i++) { steel(rr(42, 40 + i * 34, 30, 12, 4)); steel(rr(184, 40 + i * 34, 30, 12, 4)); } iron(rr(18, 14, 24, 228, 8)); iron(rr(214, 14, 24, 228, 8)); steel(rr(20, 112, 216, 18, 7)); brass(rr(112, 100, 32, 42, 8)); for (double[] q : new double[][]{{30, 34}, {226, 34}, {30, 222}, {226, 222}}) brass(gear(q[0], q[1], 8, 15, 11, 4)); });
    sprite("u_skiff", 72, () -> { Path2D h = new Path2D.Double(); h.moveTo(4, 24); h.quadTo(40, 14, 70, 36); h.quadTo(40, 58, 4, 48); h.closePath(); solid(h, WOOD_L, WOOD, WOOD_D); fill(oval(28, 36, 18, 8), c(0x5c3c2a)); Path2D sail = new Path2D.Double(); sail.moveTo(30, 8); sail.quadTo(56, 36, 30, 64); sail.quadTo(40, 36, 30, 8); solid(sail, Color.WHITE, CREAM, c(0xc9b08c), 2.6); seg(33, 10, 33, 62, WOOD_D, 3); brass(circle(50, 36, 6)); });
    sprite("u_cutter", 96, () -> { Path2D h = new Path2D.Double(); h.moveTo(5, 28); h.quadTo(54, 16, 94, 48); h.quadTo(54, 80, 5, 68); h.closePath(); solid(h, COPPER_L, COPPER, COPPER_D); planks(18, 34, 50, 28, 4, false); line(rr(18, 34, 50, 28, 4), INK, 2.4); iron(circle(44, 48, 13)); fill(circle(44, 48, 7), c(0x241820)); glow(44, 48, 9, FIRE, 160); brass(rr(66, 43, 18, 10, 4)); iron(rr(8, 40, 10, 16, 3)); for (int i = 0; i < 3; i++) { bolt(26 + i * 16, 27); bolt(26 + i * 16, 69); } });
    sprite("u_icebreaker", 140, () -> { Path2D h = new Path2D.Double(); h.moveTo(6, 40); h.lineTo(84, 30); h.quadTo(116, 44, 138, 70); h.quadTo(116, 96, 84, 110); h.lineTo(6, 100); h.closePath(); solid(h, STEEL_L, STEEL, STEEL_D, 4); solid(poly(104, 44, 139, 70, 104, 96, 112, 70), c(0xf2fbff), c(0xbfdcee), c(0x6a94b4), 3); planks(22, 50, 60, 40, 5, false); line(rr(22, 50, 60, 40, 5), INK, 2.6); copper(circle(40, 70, 14)); fill(circle(40, 70, 8), c(0x241820)); glow(40, 70, 11, FIRE, 190); steel(circle(76, 70, 17)); iron(rr(80, 60, 26, 7, 3)); iron(rr(80, 73, 26, 7, 3)); for (int i = 0; i < 4; i++) { bolt(24 + i * 18, 43); bolt(24 + i * 18, 97); } });
    sprite("wake", T, () -> { for (int i = 0; i < 3; i++) { Path2D p = new Path2D.Double(); p.moveTo(60 - i * 16, 32); p.quadTo(40 - i * 14, 30 - i * 4, 10 - i * 3, 12 - i * 2); p.moveTo(60 - i * 16, 32); p.quadTo(40 - i * 14, 34 + i * 4, 10 - i * 3, 52 + i * 2); line(p, a(Color.WHITE, 150 - i * 40), 3); } });
    // Thinking machines.
    sprite("loom", T * 2, () -> { footing(2); iron(rr(16, 16, 96, 96, 12)); fill(rr(26, 26, 76, 62, 6), c(0x2d2230)); for (int i = 0; i < 9; i++) seg(32 + i * 8, 28, 32 + i * 8, 86, i % 3 == 0 ? c(0xd1503e) : i % 3 == 1 ? c(0xf4e3c3) : c(0x4aa6a0), 2.2); for (int k = 0; k < 3; k++) { cream(rr(30, 34 + k * 17, 68, 12, 2)); for (int i = 0; i < 9; i++) if (r.nextInt(3) > 0) fill(circle(37 + i * 7, 40 + k * 17, 2), INK); } brass(rr(22, 92, 84, 12, 5)); brass(gear(20, 20, 8, 11, 8, 3)); brass(gear(108, 108, 8, 11, 8, 3)); });
    sprite("sign", T, () -> { wood(rr(28, 40, 8, 22, 3)); wood(rr(4, 8, 56, 40, 6)); fill(rr(10, 14, 44, 28, 4), c(0x2d2a34)); line(rr(10, 14, 44, 28, 4), INK, 2.4); bolts(8, 12, 48, 32); });
    // The Bramble underfoot.
    for (int k = 0; k < 4; k++) sprite("root" + k, 96, () -> { for (int i = 0; i < 7; i++) { double an = r.nextDouble() * 6.28, x = 48 + (r.nextDouble() - .5) * 22, y = 48 + (r.nextDouble() - .5) * 22, w = 7 + r.nextDouble() * 3; for (int n = 0; n < 5; n++) { double nx = x + Math.cos(an) * 11, ny = y + Math.sin(an) * 11; seg(x, y, nx, ny, a(INK, 210), w + 3); seg(x, y, nx, ny, mix(c(0x5c3a4e), c(0x4a5a34), r.nextDouble() * .8), w); if (n > 1 && r.nextInt(2) == 0) { double sa = an + (r.nextBoolean() ? 1.3 : -1.3); fill(poly(nx - 2, ny - 2, nx + Math.cos(sa) * 9, ny + Math.sin(sa) * 9, nx + 2, ny + 2), c(0xd9c69a)); } x = nx; y = ny; w *= .78; an += (r.nextDouble() - .5) * 1.1; } } for (int i = 0; i < 3; i++) { double x = 26 + r.nextDouble() * 44, y = 26 + r.nextDouble() * 44; solid(circle(x, y, 4), c(0xd88aa0), THORN, THORN_D, 1.6); } });
    sprite("pod", T * 2, () -> { for (int i = 0; i < 9; i++) { double an = i * .7; Shape th = turned(poly(64, 54, 122, 64, 64, 74), an, 64, 64); solid(th, LEAF, LEAF_D, c(0x1e3424), 2.6); } solid(blob(64, 64, 38, .06, 12, r), c(0xd8a0c0), c(0x9c5080), c(0x4a2248), 4); for (int i = 0; i < 5; i++) { Path2D p = new Path2D.Double(); p.moveTo(64, 28); p.quadTo(64 + (i - 2) * 26, 64, 64, 100); line(p, a(c(0x4a2248), 150), 2.4); } fill(oval(52, 50, 8, 11), a(Color.WHITE, 110)); });
    sprite("pod_top", T * 2, () -> { solid(blob(64, 64, 17, .12, 9, r), c(0xffe8a0), c(0xe8a24a), c(0x8c4a2a), 2.6); });
    sprite("u_thistledown", 72, () -> { for (int i = 0; i < 20; i++) { double an = i * .314; seg(36, 36, 36 + Math.cos(an) * 30, 36 + Math.sin(an) * 30, a(Color.WHITE, 190), 1.8); for (int k = -1; k <= 1; k++) seg(36 + Math.cos(an) * 26, 36 + Math.sin(an) * 26, 36 + Math.cos(an + k * .16) * 34, 36 + Math.sin(an + k * .16) * 34, a(Color.WHITE, 220), 1.4); } solid(oval(36, 36, 11, 14), c(0xd8a878), c(0x8a5a3a), c(0x4a2c20), 2.6); eyes(40, 36, 5, 2.6); });
    icon("share", () -> { seg(14, 24, 34, 12, INK, 7); seg(14, 24, 34, 36, INK, 7); seg(14, 24, 34, 12, CREAM, 3.4); seg(14, 24, 34, 36, CREAM, 3.4); for (double[] q : new double[][]{{13, 24}, {35, 11}, {35, 37}}) brass(circle(q[0], q[1], 7)); });
    icon("cards", () -> { for (int i = 2; i >= 0; i--) cream(rr(8 + i * 4, 10 + i * 4, 26, 20, 3)); for (int i = 0; i < 6; i++) fill(circle(13 + (i % 3) * 8, 16 + (i / 3) * 8, 2.2), INK); });
    icon("aim", () -> { solid(circle(24, 24, 18), c(0xfff6e0), CREAM, c(0xc9b08c)); line(circle(24, 24, 11), c(0xd1503e), 4); fill(circle(24, 24, 4.6), c(0xd1503e)); });
    icon("screen", () -> { for (int i = 0; i < 4; i++) { Shape k = turned(poly(8, 8, 22, 8, 8, 22), i * Math.PI / 2, 24, 24); solid(k, BRASS_L, BRASS, BRASS_D, 2.4); } });
  }

  public static void main(String[] args) throws Exception {
    System.setProperty("java.awt.headless", "true");
    out = new File(args[0]);
    out.mkdirs();
    land(); goods(); haulage(); gathering(); crafting(); power(); defense(); seasonal(); automata(); creatures(); shots(); effects(); ui(); goods2(); works2(); creatures2(); effects2(); third();
    StringBuilder sb = new StringBuilder();
    sb.append("package hearthwheel.core\n\nimport sgl.assets.DrawableAsset\n\n/** Every picture by its name. Written by tools/Art.java. */\ntrait Sheet {\n  this: AssetsProvider =>\n\n  def sheet: Vector[(String, DrawableAsset)] = Vector(\n");
    for (String n : names) sb.append("    \"" + n + "\" -> Assets.drawable." + camel(n) + ",\n");
    sb.setLength(sb.length() - 2);
    sb.append("\n  )\n}\n");
    java.nio.file.Files.writeString(new File(args[1]).toPath(), sb.toString());
    System.out.println(names.size() + " sprites");
    // A contact sheet, to look at.
    if (args.length > 2) {
      int cols = 22, cell = 84;
      BufferedImage sheet = new BufferedImage(cols * cell, ((names.size() + cols - 1) / cols) * cell, BufferedImage.TYPE_INT_ARGB);
      Graphics2D sg = sheet.createGraphics();
      sg.setColor(c(0x86a35c)); sg.fillRect(0, 0, sheet.getWidth(), sheet.getHeight());
      sg.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
      int i = 0;
      for (String n : names) {
        BufferedImage im = ImageIO.read(new File(out, n + ".png"));
        double k = Math.min(1.0, 78.0 / Math.max(im.getWidth(), im.getHeight()));
        int w = (int) (im.getWidth() * k), h = (int) (im.getHeight() * k);
        sg.drawImage(im, (i % cols) * cell + (cell - w) / 2, (i / cols) * cell + (cell - h) / 2, w, h, null);
        i++;
      }
      ImageIO.write(sheet, "png", new File(args[2]));
    }
  }
}
