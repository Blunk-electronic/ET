------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        BOARD / DRAW NETCHANGERS                          --
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


with ada.text_io;					use ada.text_io;
with et_colors;						use et_colors;
-- with et_primitive_objects;			use et_primitive_objects;
with et_netchangers;				use et_netchangers;
with et_netchangers.board;			use et_netchangers.board;
with et_netchanger_symbol_board;	use et_netchanger_symbol_board;
with et_text_content;				use et_text_content;
with et_alignment;					use et_alignment;


separate (et_canvas_board)


procedure draw_netchangers is

	use et_colors.board;

	brightness : type_brightness := NORMAL;
	

	-- This procedure draws the body of the netchanger
	-- at the given position:
	procedure draw_body (
		place : in type_vector_model)
	is 
		position : type_position;
	begin
		set_color_netchanger (brightness);
		-- CS draw with the color of the signal layer ?

		set_place (position, place);
		
		draw_line (netchanger_symbol_board.line_1, position,
				   linewidth_box, stroke => DO_STROKE);

		draw_line (netchanger_symbol_board.line_2, position,
				   linewidth_box, stroke => DO_STROKE);

		-- CS use just a single stroke command here ?
	end draw_body;




	-- This procedure draws the full name of the netchanger:
	procedure draw_name (
		place : in type_vector_model;
		index : in type_netchanger_id) -- 1,2,3, ...
	is
		use pac_draw_text;
		p_final : type_vector_model := place;
	begin
		-- The final position is below the body of the netchanger:
		move (p_final, DIR_DOWN, name_to_origin_offset);
		
		draw_text (
			content		=> to_content (get_netchanger_name (index)),
			size		=> name_size,
			font		=> netchanger_name_font,
			anchor		=> p_final,
			origin		=> false, -- no origin required
			rotation	=> 0.0, -- never rotated
			alignment	=> (ALIGN_CENTER, ALIGN_CENTER));
	end draw_name;
	 

	 
	 
	-- This procedure draws the number of
	-- the signal layer the netchanger is placed in:
	procedure draw_layer (
		place : in type_vector_model;
		layer : in string) -- like L4
	is
		use pac_draw_text;
		p_final : type_vector_model := place;
	begin
		-- The final position is right of the body of the netchanger:
		move (p_final, DIR_RIGHT, layer_id_to_origin_offset);
		
		draw_text (
			content		=> to_content (layer),
			size		=> layer_size,
			font		=> netchanger_name_font, -- CS use a slanted font ?
			anchor		=> p_final,
			origin		=> false, -- no origin required
			rotation	=> 0.0, -- never rotated
			alignment	=> (ALIGN_LEFT, ALIGN_CENTER));
		null;
	end draw_layer;
	 
	 
	 
	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is 
		use pac_netchangers;
		netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;


		procedure query_netchanger (
			index		: in type_netchanger_id;
			netchanger	: in type_netchanger)
		is
			position : type_vector_model := get_place (netchanger);
		begin
			-- The default brightness is NORMAL. 
			-- If the netchanger is selected, 
			-- then the brightness will be increased:
			brightness := NORMAL;
			
			-- Draw the netchanger candidate highlighted if
			-- it is selected:
			if is_selected (netchanger) then

				brightness := BRIGHT;

				-- overwrite position if netchanger is moving:
				if is_moving (netchanger) then
					position := get_object_tool_position;
				end if;
			end if;

			draw_body (position);

			draw_name (position, index);
			
			draw_layer (position, get_layer (netchanger));
		end query_netchanger;
			
			
			
	begin
		-- Iterate through the netchangers of the module:
		while has_element (netchanger_cursor) loop
			query_element (netchanger_cursor, query_netchanger'access);
			next (netchanger_cursor);
		end loop;		
	end query_module;
	

	
begin
-- 	put_line ("draw netchangers (board)");


	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_module'access);

	
end draw_netchangers;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
