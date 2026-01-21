------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         MODULE READ / VIAS                               --
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
with et_keywords;					use et_keywords;

with et_board_ops;
with et_pcb_stack;					use et_pcb_stack;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_vias;						use et_vias;
with et_drills;						use et_drills;

with et_module_read_nets;

with et_board_ops_signal_layers;	use et_board_ops_signal_layers;



package body et_module_read_via is

	use pac_generic_modules;
	use pac_geometry_2;

	
	drill 				: type_drill;
	via_category 		: et_vias.type_via_category;
	via_restring_inner	: type_restring_width; -- CS default DRC
	via_restring_outer	: type_restring_width; -- CS default DRC	
	via_layers_buried	: et_vias.type_buried_layers;
	via_layer_blind		: et_vias.type_via_layer;

	
	
	procedure read_via (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_fields_of_line)
	is
		use et_board_geometry.pac_geometry_2;
		use et_vias;
		use et_pcb_stack;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the position starting at field 2 of line
			drill.position := to_vector_model (line, 2);

		elsif kw = keyword_via_category then -- category through/buried/...
			expect_field_count (line, 2);
			via_category := to_via_category (f (line, 2));
			
		elsif kw = keyword_diameter then -- diameter 0.35
			expect_field_count (line, 2);
			drill.diameter := to_distance (f (line, 2));
			-- CS validate against dru settings
						
		elsif kw = keyword_restring_outer then -- restring_outer 0.3
			expect_field_count (line, 2);
			via_restring_outer := to_distance (f (line, 2));
			-- CS validate against dru settings
			
		elsif kw = keyword_restring_inner then -- restring_inner 0.34
			expect_field_count (line, 2);
			via_restring_inner := to_distance (f (line, 2));
			-- CS validate against dru settings
						
		elsif kw = keyword_layers then -- layers 2 3 (for buried via only)
			expect_field_count (line, 3);
			via_layers_buried := to_buried_layers (
						upper	=> f (line, 2),
						lower	=> f (line, 3),
						bottom	=> get_deepest_conductor_layer (module_cursor));
			
		elsif kw = keyword_destination then -- destination 15 (for blind via only)
			expect_field_count (line, 2);
			via_layer_blind := to_signal_layer (f (line, 2));
			-- CS exception rises if layer out of range (i.e. less than 2).
			--validate_signal_layer (via_layers_buried.lower);
			
		else
			invalid_keyword (kw);
		end if;
		
	end read_via;


			
			
			
			
			
	procedure build_via is
		use et_module_read_nets;
		use pac_vias;
	begin
		-- insert via in route.vias
		case via_category is
			when THROUGH =>
				append (route.vias, ((drill with
					category		=> THROUGH,
					restring_inner	=> via_restring_inner,
					restring_outer	=> via_restring_outer)));

			when BLIND_DRILLED_FROM_TOP =>
				-- CS validate via_layer_blind. must be higher than 
				-- deepest used layer.
				
				append (route.vias, ((drill with
					category		=> BLIND_DRILLED_FROM_TOP,
					restring_inner	=> via_restring_inner,
					restring_top	=> via_restring_outer,
					lower			=> via_layer_blind)));

			when BLIND_DRILLED_FROM_BOTTOM =>
				-- CS validate via_layer_blind. must be lower than 
				-- top layer and higher than deepest used layer.
				
				append (route.vias, ((drill with
					category		=> BLIND_DRILLED_FROM_BOTTOM,
					restring_inner	=> via_restring_inner,
					restring_bottom	=> via_restring_outer,
					upper			=> via_layer_blind)));

			when BURIED =>
				-- CS validate via_layers_buried. must be higher than 
				-- deepst used layer.
				
				append (route.vias, ((drill with
					category		=> BURIED,
					restring_inner	=> via_restring_inner,
					layers			=> via_layers_buried)));
				
		end case;

		drill := (others => <>); -- clean up for next via
		via_category := via_category_default;
		via_layers_buried := (others => <>);
		via_layer_blind := type_via_layer'first;
		-- CS
		-- via_restring_inner := DRC ?
		-- via_restring_outer := 
	end build_via;
				
				
				
end et_module_read_via;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
