import java.awt.*;
import java.awt.font.GlyphVector;
import java.awt.geom.*;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.imageio.*;
import javax.imageio.stream.ImageOutputStream;

/** The banner of the README, 1280 by 640: the same works in the four seasons,
  * side by side, under the title.
  *
  *   EGL_PLATFORM=surfaceless DISPLAY= bazel run //:banner -- /tmp/seasons
  *   java tools/Banner.java /tmp/seasons marketing/banner.jpg
  */
public class Banner {
  static final int W = 1280, H = 640;
  static final String[] SEASONS = {"spring", "summer", "autumn", "winter"};

  public static void main(String[] args) throws Exception {
    File seasons = new File(args[0]);
    BufferedImage out = new BufferedImage(W, H, BufferedImage.TYPE_INT_RGB);
    Graphics2D g = out.createGraphics();
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

    // Slanted bands, each a season of the same place.
    double slant = 140, band = W / 4.0;
    Shape[] bands = new Shape[4];
    for(int i = 0; i < 4; i++) {
      double left = i * band, right = (i + 1) * band;
      Path2D p = new Path2D.Double();
      p.moveTo(i == 0 ? -slant : left + slant / 2, 0);
      p.lineTo(i == 3 ? W + slant : right + slant / 2, 0);
      p.lineTo(i == 3 ? W + slant : right - slant / 2, H);
      p.lineTo(i == 0 ? -slant : left - slant / 2, H);
      p.closePath();
      bands[i] = p;
      Shape before = g.getClip();
      g.clip(p);
      g.drawImage(ImageIO.read(new File(seasons, "banner_" + SEASONS[i] + ".png")), 0, 0, W, H, null);
      g.setClip(before);
    }
    g.setColor(new Color(0xf4, 0xe4, 0xc4));
    g.setStroke(new BasicStroke(5f));
    for(int i = 1; i < 4; i++) g.draw(new Line2D.Double(i * band + slant / 2, 0, i * band - slant / 2, H));

    // Dark at the top, for the title, and at the bottom.
    g.setPaint(new GradientPaint(0, 0, new Color(20, 12, 18, 225), 0, 320, new Color(20, 12, 18, 0)));
    g.fillRect(0, 0, W, 320);
    g.setPaint(new GradientPaint(0, H - 120, new Color(20, 12, 18, 0), 0, H, new Color(20, 12, 18, 170)));
    g.fillRect(0, H - 120, W, 120);

    BufferedImage title = ImageIO.read(new File("assets/sprites/title.png"));
    int tw = 820, th = tw * title.getHeight() / title.getWidth();
    g.drawImage(title, (W - tw) / 2, 18, tw, th, null);

    outlined(g, "Build, guard and wind the year", new Font("Serif", Font.BOLD, 40), W / 2.0, 18 + th + 30, new Color(0xfb, 0xee, 0xd4));
    String[] names = {"Spring", "Summer", "Autumn", "Winter"};
    for(int i = 0; i < 4; i++)
      outlined(g, names[i], new Font("Serif", Font.BOLD, 30), i * band + band / 2 - slant / 2 + slant * 30 / H, H - 30, new Color(0xfb, 0xee, 0xd4));
    g.dispose();

    ImageWriter writer = ImageIO.getImageWritersByFormatName("jpg").next();
    ImageWriteParam param = writer.getDefaultWriteParam();
    param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
    param.setCompressionQuality(0.9f);
    try(ImageOutputStream stream = ImageIO.createImageOutputStream(new File(args[1]))) {
      writer.setOutput(stream);
      writer.write(null, new IIOImage(out, null, null), param);
    }
  }

  /** Centered on x, with its baseline at y. */
  static void outlined(Graphics2D g, String text, Font font, double x, double y, Color color) {
    GlyphVector glyphs = font.createGlyphVector(g.getFontRenderContext(), text);
    Rectangle2D bounds = glyphs.getVisualBounds();
    Shape shape = glyphs.getOutline((float) (x - bounds.getCenterX()), (float) y);
    g.setStroke(new BasicStroke(7f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
    g.setColor(new Color(0x3a, 0x22, 0x24));
    g.draw(shape);
    g.setColor(color);
    g.fill(shape);
  }
}
