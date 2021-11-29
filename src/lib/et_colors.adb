------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              COLORS                                      --
--                                                                          --
--                             B o d y                                      --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.text_io;				use ada.text_io;
with glib;
with cairo.pattern;
with cairo.matrix;

package body et_colors is

	function dim (
		color		: in type_color;
		brightness	: in type_dim_factor)
		return type_color 
	is
		use type color_range;
		
		b : constant color_range := color_range (brightness);
		result : type_color;
	begin
		result.red		:= color.red * b;
		result.green	:= color.green * b;
		result.blue		:= color.blue * b;
		
		return result;
	end dim;

	
	procedure set_color (
		context		: in cairo_context;
		color		: in type_color;
		brightness	: in type_brightness)
	is 
		c : type_color;
	begin
		case brightness is
			when DARK 	=>	c := dim (color, 0.25);
			when NORMAL	=>	c := dim (color, dim_factor_default);
			when BRIGHT	=>	c := dim (color, 1.0);
		end case;

		set_source_rgb (context, c.red, c.green, c.blue);
	end set_color;

	
	procedure create_fill_pattern (
		context			: in cairo_context;
		color			: in type_color;
		opacity			: in type_opacity;
		gap_brightness	: in type_dim_factor := fill_pattern_gap_brightness_default;
		style			: in type_fill_style;
		scale			: in type_scale)
	is
		use glib;
		use cairo.pattern;
		use cairo.matrix;

		zero : constant gdouble := 0.0;
		
		-- The pattern appearance must be independed of the scale of the canvas.
		-- So we need a compensation mechanism that keeps the pattern size constant. 
		-- This is the length of the gradient:
		gl : gdouble;

		-- The modifier to compensate the scale is different for straight
		-- and angular patterns. The modifier will be used to compute the 
		-- length of the gradient:
		m_0_90		: constant gdouble := 25.0;
		m_45_135	: constant gdouble := m_0_90 - 5.0;

		-- The pattern to create:
		p : cairo_pattern;

		-- The brightness of the color that is to fill the gaps between lines or dots:
		gap_color : type_color;

		procedure add_gap (offset : in gdouble) is begin
			pattern_add_color_stop_rgba (p, offset, gap_color.red, gap_color.green, gap_color.blue, 0.5);
		end add_gap;

		procedure add_foreground (offset : in gdouble) is begin
			pattern_add_color_stop_rgba (p, offset, color.red, color.green, color.blue, 0.5);
		end add_foreground;

		procedure make_gradient_0 is begin
			gl := m_0_90 / scale;

			-- gradient from left to right (in the view, in pixels)
			p := pattern_create_linear (zero, zero, zero, gl);
		end make_gradient_0;
		
		procedure make_gradient_45 is begin
			gl := m_45_135 / scale;
			
			-- gradient from top left to bottom right (in the view, in pixels)
			p := pattern_create_linear (zero, zero, gl, gl);
		end make_gradient_45;

		procedure make_gradient_90 is begin
			gl := m_0_90 / scale;

			-- gradient from top to bottom (in the view, in pixels)
			p := pattern_create_linear (zero, zero, gl, zero);
		end make_gradient_90;

		procedure make_gradient_135 is begin
			gl := m_45_135 / scale;
			
			-- gradient from top left to bottom right (in the view, in pixels)
			p := pattern_create_linear (gl, zero, zero, gl);
		end make_gradient_135;
		
	begin -- create_fill_pattern
		case style is
			when SOLID =>
				-- No pattern to generate for solid filling:
				set_source_rgba (context, color.red, color.green, color.blue, color_range (opacity));

			when STRIPED_0		=> make_gradient_0;
			when STRIPED_45		=> make_gradient_45;
			when STRIPED_90		=> make_gradient_90;
			when STRIPED_135	=> make_gradient_135;
				
			when others => null;
		end case;

		case style is
			when STRIPED_0 | STRIPED_45 | STRIPED_90 | STRIPED_135 =>

				-- Set the brightness of the color that is to fill the gaps between lines or dots:
				gap_color := dim (color, gap_brightness);
				
				add_gap (0.50);
				add_foreground (0.51);
				add_foreground (0.55);
				add_gap (0.56);
				
				set_source (context, p);
				set_extend (get_source (context), CAIRO_EXTEND_REPEAT);

			when others => null;
		end case;
		
	end create_fill_pattern;

	
end et_colors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
