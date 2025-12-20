/*
 * FreeType helper functions for Fortran binding.
 * Provides simplified C wrappers around FreeType's complex structs.
 */

#include <ft2build.h>
#include FT_FREETYPE_H
#include <stdlib.h>
#include <string.h>

/* Initialize FreeType library */
int fortty_ft_init(FT_Library *lib) {
    return FT_Init_FreeType(lib);
}

/* Cleanup FreeType library */
void fortty_ft_done(FT_Library lib) {
    FT_Done_FreeType(lib);
}

/* Load a font face at specified pixel size */
int fortty_ft_load_font(FT_Library lib, const char *path, int size_px, FT_Face *face) {
    FT_Error err = FT_New_Face(lib, path, 0, face);
    if (err) return err;

    err = FT_Set_Pixel_Sizes(*face, 0, size_px);
    if (err) {
        FT_Done_Face(*face);
        return err;
    }

    return 0;
}

/* Cleanup font face */
void fortty_ft_done_face(FT_Face face) {
    FT_Done_Face(face);
}

/* Get font metrics for terminal cell sizing */
void fortty_ft_get_metrics(FT_Face face, int *cell_width, int *cell_height,
                           int *ascender, int *descender) {
    /* For monospace fonts, use the max advance width */
    if (face->face_flags & FT_FACE_FLAG_FIXED_WIDTH) {
        *cell_width = face->max_advance_width >> 6;
    } else {
        /* Fallback: measure 'M' character */
        if (FT_Load_Char(face, 'M', FT_LOAD_DEFAULT) == 0) {
            *cell_width = face->glyph->advance.x >> 6;
        } else {
            *cell_width = face->size->metrics.max_advance >> 6;
        }
    }

    /* Height from font metrics (in 1/64 pixels, convert to pixels) */
    *ascender = face->size->metrics.ascender >> 6;
    *descender = -(face->size->metrics.descender >> 6); /* Make positive */
    *cell_height = *ascender + *descender;
}

/* Render a glyph and return its bitmap data
 * Returns 0 on success, non-zero on error.
 * Caller should NOT free the bitmap pointer - it points to FreeType's internal buffer.
 */
int fortty_ft_render_glyph(FT_Face face, unsigned int codepoint,
                           unsigned char **bitmap, int *width, int *height,
                           int *bearing_x, int *bearing_y, int *advance) {
    FT_Error err = FT_Load_Char(face, codepoint, FT_LOAD_RENDER);
    if (err) return err;

    FT_GlyphSlot g = face->glyph;

    *bitmap = g->bitmap.buffer;
    *width = g->bitmap.width;
    *height = g->bitmap.rows;
    *bearing_x = g->bitmap_left;
    *bearing_y = g->bitmap_top;
    *advance = g->advance.x >> 6;  /* Convert from 26.6 fixed point */

    return 0;
}

/* Get size of glyph bitmap without rendering (for atlas planning) */
int fortty_ft_get_glyph_size(FT_Face face, unsigned int codepoint,
                              int *width, int *height) {
    FT_Error err = FT_Load_Char(face, codepoint, FT_LOAD_BITMAP_METRICS_ONLY);
    if (err) return err;

    *width = face->glyph->bitmap.width;
    *height = face->glyph->bitmap.rows;

    /* If metrics-only didn't give us size, do full load */
    if (*width == 0 && *height == 0) {
        err = FT_Load_Char(face, codepoint, FT_LOAD_DEFAULT);
        if (err) return err;
        *width = face->glyph->bitmap.width;
        *height = face->glyph->bitmap.rows;
    }

    return 0;
}
