------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            BOARD DRAW VIA                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
-- To Do:
--
--

--with ada.text_io;					use ada.text_io;

with et_primitive_objects;			use et_primitive_objects;
with et_vias;						use et_vias;
with et_nets;						use et_nets;

with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_display.board;				use et_display.board;
with et_colors;						use et_colors;
with et_text_content;				use et_text_content;
with et_modes.board;				use et_modes.board;

with et_net_names;					use et_net_names;

with et_ratsnest;
with et_alignment;
with et_canvas_tool;


with et_board_ops_signal_layers;	use et_board_ops_signal_layers;



separate (et_canvas_board)

-- Draws a given via.
-- 1. If group is true, then the via is not highlighted
--    and its position not modified.
-- 2. If force_highlight is true, then the via will be
--    drawn highlighted in any case.
-- CS: This scheme seems complicated and should probably
-- be reworked.

procedure draw_via (
	via				: in type_via;
	net_name		: in type_net_name; -- CS: pass a cursor to a net instead ?
	group			: in boolean := false;
	force_highlight	: in boolean := false)
is
	-- By default the via is drawn with normal brightness.
	-- On caller request, this value will be overridden:
	brightness : type_brightness := NORMAL;

	-- When the restring is to be drawn then
	-- we just use a circle with a certain linewidth.
	-- The center of the circle is the position
	-- of the via.
	-- If the via is being moved, then the center will
	-- be overwritten by the tool position:
	circle : type_circle;
	linewidth : type_distance_positive;

	radius_base : type_distance_positive;

	-- The layer being drawn:
	current_layer : type_signal_layer;


	-- This procedure sets the linewidth
	-- and radius of the circle to be drawn:
	procedure set_width_and_radius (
		r : in type_restring_width)
	is begin
		linewidth := r;
		set_radius (circle, (radius_base + r / 2.0));
	end set_width_and_radius;



	-- This procedure draws the restring using
	-- the circle as described above:
	procedure draw_restring is
		use et_colors.board;
	begin
		set_color_via_restring (brightness);

		draw_circle (
			circle	=> circle,
			filled	=> NO,
			width	=> linewidth,
			stroke	=> DO_STROKE);
	end draw_restring;


	-- These flags are used to prevent objects from being drawn
	-- multple times at the same place:
	outer_restring_drawn, inner_restring_drawn, net_name_drawn,
	numbers_drawn, drill_size_drawn, cancel : boolean := false;

	-- CS display restring width ?


	-- Draws the net name right in the center of the via (no offset).
	-- The text size is set automatically with the radius of the drill:
	procedure draw_net_name is
		use et_colors.board;
		use et_alignment;

		use et_net_names;

		position : constant type_vector_model := get_center (circle);

		use pac_draw_text;
	begin
		if not net_name_drawn then

			-- The net name is displayed in a special color:
			set_color_via_net_name;

			draw_text (
				content		=> to_content (to_string (net_name)),
				size		=> via.diameter * ratio_diameter_to_text_size,
				font		=> via_text_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

			net_name_drawn := true;
		end if;
	end draw_net_name;



	-- Draws the layer numbers above the net name.
	-- The text size is set automatically with the radius of the drill:
	procedure draw_numbers (from, to : in string) is
		use et_colors.board;
		use et_alignment;
		position : type_vector_model := get_center (circle);

		offset : constant type_vector_model :=
			set (zero, +radius_base * text_position_layer_and_drill_factor);

		use pac_draw_text;
	begin
		move_by (position, offset);

		-- The layer numbers are displayed in a special color:
		set_color_via_layers;

		draw_text (
			content		=> to_content (from & "-" & to),
			size		=> via.diameter * ratio_diameter_to_text_size,
			font		=> via_text_font,
			anchor		=> position,
			origin		=> false,
			rotation	=> zero_rotation,
			alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

	end draw_numbers;



	-- Draws the drill size below the net name.
	-- The text size is set automatically with the radius of the drill:
	procedure draw_drill_size is
		use et_colors.board;
		use et_alignment;
		position : type_vector_model := get_center (circle);
		offset : type_vector_model;

		use pac_draw_text;
	begin
		if not drill_size_drawn then

			offset := set (zero, -radius_base * text_position_layer_and_drill_factor);

			move_by (position, offset);

			-- The drill size is displayed in a special color:
			set_color_via_drill_size; -- CS

			draw_text (
				content		=> to_content (to_string (via.diameter)),
				size		=> via.diameter * ratio_diameter_to_text_size,
				font		=> via_text_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

			drill_size_drawn := true;
		end if;
	end draw_drill_size;




	-- Depening on the category of the via, the order in
	-- which things are to be drawn differs:
	procedure query_category is

		procedure draw_numbers_blind_top is begin
			-- Draw the layer numbers only once:
			if not numbers_drawn then
				draw_numbers (
					from	=> "T",
					to		=> to_string (via.lower));

				numbers_drawn := true;
			end if;
		end draw_numbers_blind_top;


		procedure draw_numbers_blind_bottom is begin
			-- Draw the layer numbers only once:
			if not numbers_drawn then
				draw_numbers (
					from	=> "B",
					to		=> to_string (via.upper));

				numbers_drawn := true;
			end if;
		end draw_numbers_blind_bottom;


		procedure through_hole_via is begin
			if is_inner_layer (current_layer) then
				-- current_layer is an inner layer
				set_width_and_radius (via.restring_inner);

				inner_restring_drawn := true;
			else
				-- current_layer is an outer layer
				set_width_and_radius (via.restring_outer);

				outer_restring_drawn := true;
			end if;

			draw_restring;

			-- For a double layer board it is sufficent to draw
			-- the restring of the top or bottom layer. Double layer boards
			-- do not have inner restrings for vias.
			if is_double_layer_board then
				if outer_restring_drawn then
					cancel := true; -- causes the layer iterator to cancel
				end if;
			else
			-- For a multilayer board we need to draw only one outer restring
			-- (top or bottom, which one does not matter) and one inner restring.
			-- Once that is done, there is no need to draw the via again.
				if outer_restring_drawn and inner_restring_drawn then
					cancel := true; -- causes the layer iterator to cancel
				end if;
			end if;

			draw_net_name;
			draw_drill_size;

			-- NOTE: For a through via, no layer numbers are displayed.
		end through_hole_via;


		procedure buried_via is begin
			if via.layers.upper = current_layer
			or via.layers.lower = current_layer then
				set_width_and_radius (via.restring_inner);

				draw_restring;

				-- Since the inner restring width is the same for all
				-- inner signal layers, it is sufficent to draw only one
				-- restring.
				cancel := true;  -- causes the layer iterator to cancel

				-- Draw the layer numbers only once (cancel flag already set)
				draw_numbers (
					from	=> to_string (via.layers.upper),
					to		=> to_string (via.layers.lower));

				draw_net_name;
				draw_drill_size;
			end if;
		end buried_via;


		procedure blind_via_from_top is begin
			if current_layer = top_layer then
				set_width_and_radius (via.restring_top);
				outer_restring_drawn := true;
				draw_restring;
				draw_numbers_blind_top;
				draw_net_name;
				draw_drill_size;
			end if;

			if current_layer = via.lower then
				set_width_and_radius (via.restring_inner);
				inner_restring_drawn := true;
				draw_restring;
				draw_numbers_blind_top;
				draw_net_name;
				draw_drill_size;
			end if;

			-- At least the top restring AND one inner restring
			-- must have been drawn. After that no more restring
			-- shall be drawn.
			if outer_restring_drawn and inner_restring_drawn then
				cancel := true; -- causes the layer iterator to cancel
			end if;
		end blind_via_from_top;


		procedure blind_via_from_bottom is begin
			if current_layer = bottom_layer then
				set_width_and_radius (via.restring_bottom);
				outer_restring_drawn := true;
				draw_restring;
				draw_numbers_blind_bottom;
				draw_net_name;
				draw_drill_size;
			end if;

			if current_layer = via.upper then
				set_width_and_radius (via.restring_inner);
				inner_restring_drawn := true;
				draw_restring;
				draw_numbers_blind_bottom;
				draw_net_name;
				draw_drill_size;
			end if;

			-- At least the bottom restring AND one inner restring
			-- must have been drawn. After that no more restring
			-- shall be drawn.
			if outer_restring_drawn and inner_restring_drawn then
				cancel := true;
			end if;
		end blind_via_from_bottom;


	begin
		case via.category is
			when THROUGH =>
				through_hole_via;

			when BURIED =>
				buried_via;

			when BLIND_DRILLED_FROM_TOP =>
				blind_via_from_top;

			when BLIND_DRILLED_FROM_BOTTOM =>
				blind_via_from_bottom;

		end case;
	end query_category;



	-- If the via is member of a group being pasted,
	-- then nothing happens here. Otherwise:
	-- 1. If the via is selected, then it gets highlighted.
	-- 2. If the via is moving, then its position is set
	--    according to the tool being used.
	procedure set_brightness_and_position is
	begin
		if not group then

			-- Overwrite the via position (circle.center) if the
			-- via is selected and being moved:
			if is_selected (via) then

				-- A selected via must be highlighted:
				brightness := BRIGHT;

				if is_moving (via) then
					set_center (circle, get_object_tool_position);
				end if;
			end if;
		end if;
	end set_brightness_and_position;



	-- Iterates through the signal layers and draws
	-- the components of the via in each layer.
	-- If displaying vias is disabled, then nothing happens here:
	procedure iterate_layers is begin
		if vias_enabled then

			-- Iterate all conductor layers starting at the bottom layer and ending
			-- with the top layer:
			for ly in reverse top_layer .. bottom_layer loop

				-- Draw the layer only if it is enabled. Otherwise skip the layer:
				if conductor_enabled (ly) then

					-- Set the layer being drawn:
					current_layer := ly;

					query_category;
				end if;

				-- If the cancel flag has been set after drawing the via,
				-- then exit this iteration. This prevents objects from begin
				-- drawn multiple times:
				if cancel then
					exit;
				end if;

			end loop;
		end if;
	end iterate_layers;



begin
	-- Set the radius and the center of the circle:
	radius_base := via.diameter / 2.0;
	set_center (circle, via.position);

	set_brightness_and_position;

	-- Override the brightness if the caller requested so:
	if force_highlight then
		brightness := BRIGHT;
	end if;

	-- Draw the via in the signal layers:
	iterate_layers;

end draw_via;



-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
