------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE READ / NETCHANGERS                           --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_module_instance;			use et_module_instance;
with et_keywords;					use et_keywords;

with et_schematic_geometry;
with et_board_geometry;
with et_pcb_signal_layers;			use et_pcb_signal_layers;

with et_schematic_coordinates;
with et_netchangers;				use et_netchangers;
with et_net_names;					use et_net_names;

with et_general_rw;					use et_general_rw;



package body et_module_read_netchangers is

	use pac_generic_modules;


	netchanger		: type_netchanger;
	netchanger_id	: type_netchanger_id := type_netchanger_id'first;

	
	procedure read_netchanger (
		line : in type_fields_of_line)
	is
		use et_schematic_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_schematic_coordinates;	
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name 1, 2, 304, ...
			expect_field_count (line, 2);
			netchanger_id := to_netchanger_id (f (line, 2));
			
		elsif kw = keyword_position_in_schematic then -- position_in_schematic sheet 1 x 1.000 y 5.555
			expect_field_count (line, 7);

			-- extract position (in schematic) starting at field 2
			netchanger.position_sch := to_position (line, 2);

		elsif kw = keyword_rotation_in_schematic then -- rotation_in_schematic 180.0
			expect_field_count (line, 2);
			set_rotation (netchanger.position_sch, pac_geometry_2.to_rotation (f (line, 2)));

		elsif kw = keyword_position_in_board then -- position_in_board x 55.000 y 7.555
			expect_field_count (line, 5);

			-- extract position (in board) starting at field 2
			netchanger.position_brd := to_vector_model (line, 2);

		elsif kw = keyword_layer then -- layer 3 (signal layer in board)
			expect_field_count (line, 2);
			netchanger.layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer (netchanger.layer);
			
		else
			invalid_keyword (kw);
		end if;
	end read_netchanger;


		
	
	
	
	
	
	procedure insert_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		
		procedure insert_netchanger (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			inserted : boolean;
			use pac_netchangers;
			cursor : pac_netchangers.cursor;
		begin
			log (text => "netchanger " & to_string (netchanger_id), level => log_threshold + 2);

			-- insert netchanger in container netchangers:
			insert (
				container	=> module.netchangers,
				key			=> netchanger_id,
				new_item	=> netchanger,
				inserted	=> inserted,
				position	=> cursor);

			-- A netchanger name must be unique:
			if not inserted then
				log (ERROR, "netchanger id" & to_string (netchanger_id) 
					& " already used !", console => true);
				raise constraint_error;
			end if;
			
			-- clean up for next netchanger
			netchanger_id := type_netchanger_id'first;
			netchanger := (others => <>);
		end insert_netchanger;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert netchanger",
			level => log_threshold);
			
		log_indentation_up;
		
		generic_modules.update_element (module_cursor, insert_netchanger'access);
		log_indentation_down;
	end insert_netchanger;
	
	
	
	
end et_module_read_netchangers;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
