------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            BOARD COLORS                                  --
--                                                                          --
--                               B o d y                                    --
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

package body et_colors.board is

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
		context		: in cairo_context;
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
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			via.red,
			via.green,
			via.blue,
			color_range (opacity));
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
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				set_source_rgba (
					context, 
					silkscreen_top.red,
					silkscreen_top.green,
					silkscreen_top.blue,
					color_range (opacity));

			when BOTTOM =>
				set_source_rgba (
					context, 
					silkscreen_bottom.red,
					silkscreen_bottom.green,
					silkscreen_bottom.blue,
					color_range (opacity));
		end case;
	end set_color_silkscreen;

	procedure set_color_assy_doc (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				set_source_rgba (
					context, 
					assy_doc_top.red,
					assy_doc_top.green,
					assy_doc_top.blue,
					color_range (opacity));

			when BOTTOM =>
				set_source_rgba (
					context, 
					assy_doc_bottom.red,
					assy_doc_bottom.green,
					assy_doc_bottom.blue,
					color_range (opacity));
		end case;
	end set_color_assy_doc;

	procedure set_color_stop_mask (
		context : in cairo_context;
		face	: in type_face;
		scale	: in type_scale;
		opacity : in type_opacity := default_opacity) is 
	begin
		case face is
			when TOP =>
				create_fill_pattern (
					context		=> context,
					color		=> stop_mask_top,
					opacity		=> opacity,
					style		=> stop_mask_fill,
					scale		=> scale);

			when BOTTOM =>
				create_fill_pattern (
					context		=> context,
					color		=> stop_mask_bottom,
					opacity		=> opacity,
					style		=> stop_mask_fill,
					scale		=> scale);
		end case;
	
	end set_color_stop_mask;

	procedure set_color_stencil (
		context : in cairo_context;
		face	: in type_face;
		scale	: in type_scale;
		opacity : in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				create_fill_pattern (
					context		=> context,
					color		=> stencil_top,
					opacity		=> opacity,
					style		=> stencil_fill,
					scale		=> scale);

			when BOTTOM =>
				create_fill_pattern (
					context		=> context,
					color		=> stencil_bottom,
					opacity		=> opacity,
					style		=> stencil_fill,
					scale		=> scale);
				
		end case;
	end set_color_stencil;

	procedure set_color_keepout (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity)
	is begin
		case face is
			when TOP =>
				set_source_rgba (
					context, 
					keepout_top.red,
					keepout_top.green,
					keepout_top.blue,
					color_range (opacity));

			when BOTTOM =>
				set_source_rgba (
					context, 
					keepout_bottom.red,
					keepout_bottom.green,
					keepout_bottom.blue,
					color_range (opacity));
		end case;
	end set_color_keepout;

	procedure set_color_route_restrict (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			route_restrict.red,
			route_restrict.green,
			route_restrict.blue,
			color_range (opacity));
	end set_color_route_restrict;

	procedure set_color_via_restrict (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin		
		set_source_rgba (
			context, 
			via_restrict.red,
			via_restrict.green,
			via_restrict.blue,
			color_range (opacity));
	end set_color_via_restrict;

	procedure set_color_conductor (
		context : in cairo_context;
		layer	: in type_signal_layer;
		opacity : in type_opacity := default_opacity)
	is begin
		set_source_rgba (
			context, 
			conductors (layer).red,
			conductors (layer).green,
			conductors (layer).blue,
			color_range (opacity));
	end set_color_conductor;

	procedure set_color_terminal_name (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin
		set_source_rgba (
			context, 
			terminal_names.red,
			terminal_names.green,
			terminal_names.blue,
			color_range (opacity));
	end set_color_terminal_name;
		
	procedure set_color_tht_pad (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity)
	is begin
		set_source_rgba (
			context, 
			tht_pads.red,
			tht_pads.green,
			tht_pads.blue,
			color_range (opacity));
	end set_color_tht_pad;

	
	
end et_colors.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
