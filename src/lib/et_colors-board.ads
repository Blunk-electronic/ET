------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            BOARD COLORS                                  --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
with et_pcb_sides;				use et_pcb_sides;
with et_pcb_coordinates_2;		use et_pcb_coordinates_2;

with et_canvas_board_2;

package et_colors.board is

	use et_canvas_board_2.pac_canvas;
	-- The global context is now visible
	-- for all procedures that set the color.
	-- CS remove the useless argument "context".


	type type_fill_style is (
		SOLID, 
		STRIPED_0,
		STRIPED_45,
		STRIPED_90,
		STRIPED_135
-- CS DOTTED_SPARSE,
-- CS DOTTED_MEDIUM,
-- CS DOTTED_DENSE,
-- CS HATCHED_0,
-- CS HATCHED_45
		);


	
	-- CS: These values may be overwitten by user specific colors and fill
	-- styles in the future:
	
	cursor				: type_color := white; -- CS currently ignored
	background 			: type_color := black;
	frame				: type_color := white;
	outline				: type_color := gray;
	grid 				: type_color := gray;	
	origin				: type_color := gray;	
	placeholders		: type_color := gray;

	ratsnest			: type_color := yellow;
	
	via_restring		: type_color := green;
	via_layers			: type_color := gray; -- the layers the via is connecting
	via_net_name		: type_color := gray; -- the net where the via is part of
	via_drill_size		: type_color := gray; -- the drill size of the via
	
	terminal_names		: type_color := gray;
	tht_pads			: type_color := green;
	
	silkscreen_top		: type_color := gray;
	silkscreen_bottom	: type_color := gray;	

	assy_doc_top		: type_color := yellow;
	assy_doc_bottom		: type_color := yellow;	

	stop_mask_top		: type_color := green;
	stop_mask_bottom	: type_color := green;	
	stop_mask_fill		: type_fill_style := STRIPED_45;

	stencil_top			: type_color := gray;
	stencil_bottom		: type_color := gray;	
	stencil_fill		: type_fill_style := STRIPED_135;
	
	keepout_top			: type_color := orange;
	keepout_bottom		: type_color := orange;	

	route_restrict		: type_color := pink;
	via_restrict		: type_color := pink;

	-- The colors of signal layers:
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




	fill_pattern_gap_brightness_default : constant type_dim_factor := 0.5;
	
	-- Creates a fill pattern in the given context:
	procedure create_fill_pattern (
		context			: in cairo_context;
		color			: in type_color;		-- the color of the pattern
		opacity			: in type_opacity;		-- the opacity of the pattern
		-- background	: in type_color; ?

		-- the brightness of the gaps betweeen lines and dots:
		gap_brightness	: in type_dim_factor := fill_pattern_gap_brightness_default;
		
		style			: in type_fill_style);	-- the style (solid, striped, dotted)
		-- scale			: in type_scale);		-- the scale of the canvas


	

	procedure set_color_cursor (context : in cairo_context);

	
	procedure set_color_background (
		opacity : in type_opacity := default_opacity);



	-- Sets the given color, brightness and opacity in the given context:
	procedure set_color (
		color		: in type_color;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);


	
	procedure set_color_frame (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_origin (
		brightness	: in type_brightness := brightness_default);


	procedure set_color_ratsnest (
		brightness	: in type_brightness := brightness_default);

	
	-- CS add argument for brightness
	-- to the follwing subprograms ?
	
	procedure set_color_outline (
		brightness	: in type_brightness := brightness_default);
	

-- VIAS

	procedure set_color_via_restring (
		brightness	: in type_brightness := brightness_default;
		opacity		: in type_opacity := default_opacity);

	
	procedure set_color_via_layers (
		opacity : in type_opacity := default_opacity);

	
	procedure set_color_via_net_name (
		opacity : in type_opacity := default_opacity);

	
	procedure set_color_via_drill_size (
		opacity : in type_opacity := default_opacity);

	
	
	procedure set_color_silkscreen (
		face		: in type_face;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity);

	
	procedure set_color_assy_doc (
		face		: in type_face;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity);

	
	procedure set_color_stop_mask (
		face		: in type_face;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity);

	
	procedure set_color_stencil (
		face		: in type_face;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity);

	
	procedure set_color_keepout (
		face		: in type_face;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);

	
	procedure set_color_route_restrict (
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);

	
	procedure set_color_via_restrict (
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);

	
	procedure set_color_conductor (
		layer		: in type_signal_layer;
		brightness	: in type_brightness;
		opacity 	: in type_opacity := default_opacity);

	
	procedure set_color_terminal_name (
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);

	
	procedure set_color_tht_pad (
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);


	

-- 	procedure set_color_placeholders (context : in cairo_context);	
-- 	procedure set_color_submodules (context : in cairo_context);	

end et_colors.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
