------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              MATERIAL                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
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
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

-- with et_general;				use et_general;
-- 
-- with et_coordinates;
-- with et_libraries;
-- with assembly_variants;
-- with et_string_processing;
-- with et_pcb;
-- with et_pcb_coordinates;
-- with submodules;
-- with numbering;

package body material is
	
	function to_string (name : in type_file_name.bounded_string) return string is begin
		return type_file_name.to_string (name);
	end;
	
	function to_file_name (name : in string) return type_file_name.bounded_string is begin
		return type_file_name.to_bounded_string (name);
	end;


-- 		column_component	: constant string (1 .. 9) := "COMPONENT";
-- 		column_value		: constant string (1 .. 5) := "VALUE";
-- 		column_generic_name	: constant string (1 ..12) := "GENERIC_NAME";
-- 		column_package		: constant string (1 .. 7) := "PACKAGE";
-- 		column_author		: constant string (1 .. 6) := "AUTHOR";
-- 		column_bom			: constant string (1 .. 3) := "BOM";
-- 		column_commissioned	: constant string (1 ..12) := "COMMISSIONED";
-- 		column_purpose		: constant string (1 .. 7) := "PURPOSE";
-- 		column_part_code	: constant string (1 .. 9) := "PART_CODE"; -- CS: make sure stock_manager can handle it. former PART_CODE_BEL
-- 		column_part_code_ext: constant string (1 ..13) := "PART_CODE_EXT"; -- not used
-- 		column_updated		: constant string (1 .. 7) := "UPDATED";
-- 
-- 		procedure query_components (
-- 			module_name : in et_coordinates.type_submodule_name.bounded_string;
-- 			module		: in type_module) is
-- 		
-- 			component : type_components_schematic.cursor := module.components.first;
-- 
-- 		begin -- query_components
-- 			log_indentation_up;
-- 			while component /= type_components_schematic.no_element loop
-- 
-- 				-- We ignore all virtual components like power flags, power symbols, ...
-- 				--if component_appearance (component) = sch_pcb then
-- 				if et_libraries."=" (element (component).appearance, et_libraries.sch_pcb) then
-- 
-- 					if et_schematic."=" (bom (component), et_schematic.YES) then
-- 						log (text => et_libraries.to_string (key (component)), level => log_threshold + 2);
-- 
-- 						-- CS: warning if netchanger/net-ties occur here. they should have the bom flag set to NO.
-- 						et_csv.reset_column;
-- 						
-- 						put_field (file => bom_handle, text => et_libraries.to_string (key (component))); -- R6
-- 						put_field (file => bom_handle, text => et_libraries.to_string (element (component).value)); -- 100R
-- 						put_field (file => bom_handle, text => et_libraries.to_string (element (component).generic_name)); -- RESISTOR
-- 						put_field (file => bom_handle, text => et_libraries.to_string (to_package_name (
-- 																library_name => element (component).library_name,
-- 																generic_name => element (component).generic_name,
-- 																package_variant => element (component).variant)));
-- 						put_field (file => bom_handle, text => et_libraries.to_string (element (component).author));
-- 						put_field (file => bom_handle, text => to_string (element (component).commissioned));
-- 						put_field (file => bom_handle, text => et_libraries.to_string (element (component).purpose));
-- 						put_field (file => bom_handle, text => et_libraries.to_string (element (component).partcode));
-- 
-- 						-- CS: This is an empty field. it is reserved for the attribute "PART_CODE_EXT" 
-- 						-- which is currently not supported:
-- 						put_field (file => bom_handle, text => "");
-- 
-- 						put_field (file => bom_handle, text => to_string (element (component).updated));
-- 						put_lf    (file => bom_handle, field_count => et_csv.column);
-- 
-- 					end if;
-- 				end if;
-- 
-- 				next (component);
-- 			end loop;
-- 				
-- 			log_indentation_down;
-- 		end query_components;
-- 		
-- 	begin -- export_bom
-- 
-- 
-- 			-- CS: A nice header should be placed. First make sure stock_manager can handle it.
-- 
-- 			-- write the BOM table header
-- 			et_csv.reset_column;
-- 			put_field (file => bom_handle, text => column_component);
-- 			put_field (file => bom_handle, text => column_value);
-- 			put_field (file => bom_handle, text => column_generic_name);
-- 			put_field (file => bom_handle, text => column_package);
-- 			put_field (file => bom_handle, text => column_author);
-- 			put_field (file => bom_handle, text => column_bom);
-- 			put_field (file => bom_handle, text => column_commissioned);
-- 			put_field (file => bom_handle, text => column_purpose);
-- 			put_field (file => bom_handle, text => column_part_code);
-- 			put_field (file => bom_handle, text => column_part_code_ext);
-- 			put_field (file => bom_handle, text => column_updated);
-- 			put_lf    (file => bom_handle, field_count => et_csv.column);
-- 
-- 			query_element (
-- 				position	=> module_cursor,
-- 				process		=> query_components'access);
-- 
-- 			-- CS: A list end mark should be placed. First make sure stock_manager can handle it.
-- 			-- put_line (bom_handle, comment_mark & " end of list");
-- 			

-- 			log_indentation_down;
-- 			next (module_cursor);
-- 		end loop;
-- 
-- 		log_indentation_down;
-- 	end export_bom;

	
end material;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
