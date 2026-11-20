// One canvas for the life of the page, made on first use.
let context = null;

export const textWidthImpl = (font, written) => {
  if (context === null) context = document.createElement("canvas").getContext("2d");
  context.font = font;
  return context.measureText(written).width;
};
