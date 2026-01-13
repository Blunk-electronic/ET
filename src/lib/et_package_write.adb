------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PACKAGE WRITE                                 --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.text_io;				use ada.text_io;
with ada.exceptions;

with et_directory_and_file_ops;
with et_general_rw;						use et_general_rw;

with et_design_rules_board;				use et_design_rules_board;
with et_string_processing;				use et_string_processing;
with et_time;							use et_time;
with et_system_info;
with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_package_sections;				use et_package_sections;

with et_package_write_meta;				use et_package_write_meta;
with et_package_write_silkscreen;		use et_package_write_silkscreen;
with et_package_write_assy_doc;			use et_package_write_assy_doc;
with et_package_write_keepout;			use et_package_write_keepout;
with et_package_write_stopmask;			use et_package_write_stopmask;
with et_package_write_stencil;			use et_package_write_stencil;
with et_package_write_route_restrict;	use et_package_write_route_restrict;
with et_package_write_via_restrict;		use et_package_write_via_restrict;
with et_package_write_conductors;		use et_package_write_conductors;
with et_package_write_holes;			use et_package_write_holes;
with et_package_write_terminals;		use et_package_write_terminals;


package body et_package_write is

	

	procedure write_package (
		file_name 		: in pac_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac							   
		packge			: in type_package_model; -- the actual package model
		log_threshold	: in type_log_level) 
	is
		file_handle : ada.text_io.file_type;
		
	begin
		log (text => to_string (file_name), level => log_threshold);
		log_indentation_up;
		
		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_system_info.system_name & " package");
		put_line (comment_mark & " " & get_date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		-- CS sort the follwing actions by their importance:
		write_meta (packge, log_threshold + 1);
		write_silkscreen (packge, log_threshold + 1);		
		write_assy_doc (packge, log_threshold + 1);
		write_keepout (packge, log_threshold + 1);
		write_conductors (packge, log_threshold + 1);
		write_stopmask (packge, log_threshold + 1);
		write_stencil (packge, log_threshold + 1);
		
		write_route_restrict (packge, log_threshold + 1);
		write_via_restrict (packge, log_threshold + 1);
		
		write_holes (packge, log_threshold + 1);

		write_terminals (packge, log_threshold + 1);
		-- incl. pad properties, drill sizes, millings, ...


		
		-- write footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " package model file end");
		new_line;
		
		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		log_indentation_down;
		

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			log_indentation_down;
		
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;

	end write_package;
	
		
	
end et_package_write;
