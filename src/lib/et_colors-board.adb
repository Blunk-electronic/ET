------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            BOARD COLORS                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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


with glib;
with cairo.pattern;
with cairo.matrix;

package body et_colors.board is


	procedure create_fill_pattern (
		context			: in cairo_context;
		color			: in type_color;
		opacity			: in type_opacity;
		gap_brightness	: in type_dim_factor := fill_pattern_gap_brightness_default;
		style			: in type_fill_style)
		-- scale			: in type_scale)
	is
		use glib;
		use cairo.pattern;
		use cairo.matrix;

		zero : constant gdouble := 0.0;
		
		-- The pattern appearance must be independed of the zoom-factor of the canvas.
		-- So we need a compensation mechanism that keeps the pattern size constant. 
		-- This is the length of the gradient:
		gl : gdouble;

		-- The modifier to compensate the zoom-factor is different for straight
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
			gl := m_0_90 / gdouble (S);

			-- gradient from left to right (in the view, in pixels)
			p := pattern_create_linear (zero, zero, zero, gl);
		end make_gradient_0;
		
		procedure make_gradient_45 is begin
			gl := m_45_135 / gdouble (S);
			
			-- gradient from top left to bottom right (in the view, in pixels)
			p := pattern_create_linear (zero, zero, gl, gl);
		end make_gradient_45;

		procedure make_gradient_90 is begin
			gl := m_0_90 / gdouble (S);

			-- gradient from top to bottom (in the view, in pixels)
			p := pattern_create_linear (zero, zero, gl, zero);
		end make_gradient_90;

		procedure make_gradient_135 is begin
			gl := m_45_135 / gdouble (S);
			
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


	
	procedure set_color_cursor (context : in cairo_context) is begin		
		set_source_rgb (
			context, 
			cursor.red,
			cursor.green,
			cursor.blue);
	end set_color_cursor;

	
	procedure set_color_background (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin
		set_source_rgba (
			context, 
			background.red,
			background.green,
			background.blue,
			color_range (opacity));
	end set_color_background;

	
	procedure set_color_frame (
		-- context		: in cairo_context;
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable frame
		
		set_color (context, frame, brightness);
	end set_color_frame;

	
	procedure set_color_origin (
		context		: in cairo_context;
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable origin
		
		set_color (context, origin, brightness);
	end set_color_origin;


	procedure set_color_ratsnest (
		context 	: in cairo_context;
		brightness	: in type_brightness := brightness_default)
	is begin
		set_color (context, ratsnest, brightness);
	end set_color_ratsnest;

	
	
	procedure set_color_outline (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			outline.red,
			outline.green,
			outline.blue,
			color_range (opacity));
	end set_color_outline;

	
-- VIAS

	procedure set_color_vias (
		context		: in cairo_context;
		brightness	: in type_brightness := brightness_default;
		opacity		: in type_opacity := default_opacity)
	is begin		
		set_color (context, via, brightness, opacity);
	end set_color_vias;

	
	procedure set_color_via_layers (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			via_layers.red,
			via_layers.green,
			via_layers.blue,
			color_range (opacity));
	end set_color_via_layers;

	
	procedure set_color_via_net_name (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			via_net_name.red,
			via_net_name.green,
			via_net_name.blue,
			color_range (opacity));
	end set_color_via_net_name;

	
	procedure set_color_via_drill_size (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			via_drill_size.red,
			via_drill_size.green,
			via_drill_size.blue,
			color_range (opacity));
	end set_color_via_drill_size;
	
	
	procedure set_color_silkscreen (
		context 	: in cairo_context;
		face		: in type_face;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				set_color (context, silkscreen_top, brightness, opacity);

			when BOTTOM =>
				set_color (context, silkscreen_bottom, brightness, opacity);
		end case;
	end set_color_silkscreen;

	
	procedure set_color_assy_doc (
		context 	: in cairo_context;
		face		: in type_face;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				set_color (context, assy_doc_top, brightness, opacity);

			when BOTTOM =>
				set_color (context, assy_doc_bottom, brightness, opacity);
		end case;
	end set_color_assy_doc;

	
	procedure set_color_stop_mask (
		context 	: in cairo_context;
		face		: in type_face;
		-- scale		: in type_scale;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity) 
	is begin
		case face is
			when TOP =>
				create_fill_pattern (
					context		=> context,
					color		=> dim (stop_mask_top, brightness),
					opacity		=> opacity,
					style		=> stop_mask_fill);
					-- scale		=> scale);

			when BOTTOM =>
				create_fill_pattern (
					context		=> context,
					color		=> dim (stop_mask_bottom, brightness),
					opacity		=> opacity,
					style		=> stop_mask_fill);
					-- scale		=> scale);
		end case;	
	end set_color_stop_mask;
	

	procedure set_color_stencil (
		context 	: in cairo_context;
		face		: in type_face;
		-- scale		: in type_scale;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				create_fill_pattern (
					context		=> context,
					color		=> dim (stencil_top, brightness),
					opacity		=> opacity,
					style		=> stencil_fill);
					-- scale		=> scale);

			when BOTTOM =>
				create_fill_pattern (
					context		=> context,
					color		=> dim (stencil_bottom, brightness),
					opacity		=> opacity,
					style		=> stencil_fill);
					-- scale		=> scale);
				
		end case;
	end set_color_stencil;
	

	procedure set_color_keepout (
		context 	: in cairo_context;
		face		: in type_face;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				set_color (context, keepout_top, brightness, opacity);

			when BOTTOM =>
				set_color (context, keepout_bottom, brightness, opacity);
		end case;
	end set_color_keepout;

	
	procedure set_color_route_restrict (
		context		: in cairo_context;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is begin		
		set_color (context, route_restrict, brightness, opacity);
	end set_color_route_restrict;

	
	procedure set_color_via_restrict (
		context		: in cairo_context;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is begin		
		set_color (context, via_restrict, brightness, opacity);
	end set_color_via_restrict;

	
	procedure set_color_conductor (
		context 	: in cairo_context;
		layer		: in type_signal_layer;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is begin
		set_color (context, conductors (layer), brightness, opacity);
	end set_color_conductor;
	

	procedure set_color_terminal_name (
		context		: in cairo_context;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is begin
		set_color (context, terminal_names, brightness, opacity);
	end set_color_terminal_name;
	
		
	procedure set_color_tht_pad (
		context		: in cairo_context;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is begin
		set_color (context, tht_pads, brightness, opacity);
	end set_color_tht_pad;

	
	
end et_colors.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
