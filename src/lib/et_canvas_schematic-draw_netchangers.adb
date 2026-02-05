------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       SCHEMATIC / DRAW NETCHANGERS                       --
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
--                                                                          --
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
-- - implement drawing netchangers


with ada.text_io;					use ada.text_io;

with et_colors;							use et_colors;

with et_net_linewidth;					use et_net_linewidth;
with et_netchanger_symbol_schematic;	use et_netchanger_symbol_schematic;
with et_netchangers;					use et_netchangers;
-- with et_primitive_objects;			use et_primitive_objects;
-- with et_alignment;



separate (et_canvas_schematic)

procedure draw_netchangers is

	use et_colors.schematic;
	
	brightness : type_brightness := NORMAL;

	

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
			-- place : type_vector_model;
			-- rotation : type_rotation;
			
			symbol : type_netchanger_symbol := netchanger_symbol_default;
		begin
			-- place := get_place (netchanger.position_sch);
			-- rotation := get_rotation (netchanger.position_sch);
			
			-- CS color, name, ports, origin
			-- 
			
			set_color_symbols (brightness);
			
			draw_arc (
				arc		=> symbol.arc,
				pos		=> type_position (netchanger.position_sch),
				width	=> net_linewidth,
				do_stroke => true
				);
			
			null;
			
			-- set_color_ports (brightness);
		end query_netchanger;
		
									   
	begin		
		-- Iterate through the netchangers of the module:
		while has_element (netchanger_cursor) loop
			query_element (netchanger_cursor, query_netchanger'access);
			next (netchanger_cursor);
		end loop;
	end query_module;

	
	
begin
-- 	put_line ("draw netchangers (schematic)");


	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_module'access);

	-- CS: draw_netchanger_being_added
	
end draw_netchangers;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
