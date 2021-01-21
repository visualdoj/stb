# pascal stb

This is pascal port of [stb](https://github.com/nothings/stb) -
single-file public domain (or MIT licensed) libraries.

## Status

library    | status | category | LoC | description
--------------------- | ---- | -------- | --- | --------------------------------
**[stb_sprintf.pas](stb_sprintf.pas)** | ✓1.09 | utility | ~2077 | fast `sprintf`, `snprintf`
`stb_vorbis.pas` | - | audio | - | decode ogg vorbis files from file/memory to float/16-bit signed output
`stb_image.pas` | - | graphics | - | image loading/decoding from file/memory: JPG, PNG, TGA, BMP, PSD, GIF, HDR, PIC
`stb_truetype.pas` | - | graphics | - | parse, decode, and rasterize characters from truetype fonts
`stb_image_write.pas` | - | graphics | - | image writing to disk: PNG, TGA, BMP
`stb_image_resize.pas` | - | graphics | - | resize images larger/smaller with good quality
`stb_rect_pack.pas` | - | graphics | - | simple 2D rectangle packer with decent quality
`stb_ds.pas` | - | utility | - | typesafe dynamic array and hash tables for C, will compile in C++
`stretchy_buffer.pas` | - | utility | - | typesafe dynamic array for C (i.e. approximation to vector<>), doesn't compile as C++
`stb_textedit.pas` | - | user&nbsp;interface | - | guts of a text editor for games etc implementing them from scratch
`stb_voxel_render.pas` | - | 3D&nbsp;graphics | - | Minecraft-esque voxel rendering "engine" with many more features
`stb_dxt.pas` | - | 3D&nbsp;graphics | - | Fabian "ryg" Giesen's real-time DXT compressor
`stb_perlin.pas` | - | 3D&nbsp;graphics | - | revised Perlin noise (3D input, 1D output)
`stb_easy_font.pas` | - | 3D&nbsp;graphics | - | quick-and-dirty easy-to-deploy bitmap font for printing frame rate, etc
`stb_tilemap_editor.pas` | - | game&nbsp;dev | - | embeddable tilemap editor
`stb_herringbone_wa...` | - | game&nbsp;dev | - | herringbone Wang tile map generator
`stb_c_lexer.pas` | - | parsing | - | simplify writing parsers for C-like languages
`stb_divide.pas` | - | math | - | more useful 32-bit modulus e.g. "euclidean divide"
`stb_connected_comp...` | - | misc | - | incrementally compute reachability on grids
`stb.h` | - | misc | - | helper functions for C, mostly redundant in C++; basically author's personal stuff
`stb_leakcheck.h` | - | misc | - | quick-and-dirty malloc/free leak-checking
`stb_include.h` | - | misc | - | implement recursive #include support, particularly for GLSL
