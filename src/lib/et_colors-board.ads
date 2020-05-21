------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            BOARD COLORS                                  --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
with cairo;						use cairo;
-- with cairo.pattern;				use cairo.pattern;
-- with gtkada.style;

package et_colors.board is

	cursor			: type_color := white;
	background 		: type_color := black;
	frame			: type_color := white;
	outline			: type_color := white;
	grid 			: type_color := gray;	
	origin			: type_color := gray;	
	placeholders	: type_color := white;
	via				: type_color := green;
	terminal_names	: type_color := white;
	tht_pads		: type_color := green;
	
	silkscreen_top		: type_color := white;
	silkscreen_bottom	: type_color := white;	

	assy_doc_top		: type_color := yellow;
	assy_doc_bottom		: type_color := yellow;	

	stop_mask_top		: type_color := green;
	stop_mask_bottom	: type_color := green;	

	stencil_top			: type_color := gray;
	stencil_bottom		: type_color := gray;	

	keepout_top			: type_color := orange;
	keepout_bottom		: type_color := orange;	

	route_restrict		: type_color := red;
	via_restrict		: type_color := red;

	type type_conductors is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_color;

	-- These are the default colors for conductor layers 1 .. 4. CS: needs refinement
	conductors : type_conductors := (
		1		=> red,
		2		=> yellow,
		3		=> green,
		4		=> blue,
		others	=> blue);
	
-- 	submodules		: type_color := mangenta; -- boundaries


	procedure set_color_cursor (context : in cairo_context);
	procedure set_color_background (context : in cairo_context);
	procedure set_color_background_transparent (context : in cairo_context);	
	procedure set_color_frame (context : in cairo_context);	
	procedure set_color_origin (context : in cairo_context);
	
	procedure set_color_outline (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity);
	
	procedure set_color_vias (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity);

	procedure set_color_silkscreen (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity);

	procedure set_color_assy_doc (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity);

	procedure set_color_stop_mask (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity);

	procedure set_color_stencil (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity);

	procedure set_color_keepout (
		context : in cairo_context;
		face	: in type_face;
		opacity : in type_opacity := default_opacity);

	procedure set_color_route_restrict (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity);

	procedure set_color_via_restrict (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity);

	procedure set_color_conductor (
		context : in cairo_context;
		layer	: in type_signal_layer;
		opacity : in type_opacity := default_opacity);

	procedure set_color_terminal_name (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity);

	procedure set_color_tht_pad (
		context : in cairo_context;
		opacity : in type_opacity := default_opacity);

	

-- 	procedure set_color_placeholders (context : in cairo_context);	
-- 	procedure set_color_submodules (context : in cairo_context);	

end et_colors.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
