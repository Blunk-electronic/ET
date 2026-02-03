------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE READ / PACKAGE VARIANT                       --
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
--
-- To Do:
-- - clean up
--
--

with ada.text_io;					use ada.text_io;
with ada.strings; 					use ada.strings;

with ada.directories;
with ada.exceptions;

with et_file_sections;				use et_file_sections;
with et_package_read;

with et_package_name;				use et_package_name;

with et_package_library;
with et_package_variant_name;		use et_package_variant_name;

with et_package_model_name;			use et_package_model_name;
with et_keywords;					use et_keywords;



package body et_device_read_package_variant is
	
	
	use pac_package_variant_name;
	variant_name	: pac_package_variant_name.bounded_string; -- N, D


		
		
	procedure read_package_variant (
		line 			: in type_fields_of_line;
   		check_layers	: in type_layer_check := (check => NO);
		log_threshold	: in type_log_level)
	is
		use ada.directories;
		use et_package_read;		
		use et_package_library;
		
		kw : string := f (line, 1);
		
		package_model_name : pac_package_model_file.bounded_string;		
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name D
			expect_field_count (line, 2);
			check_variant_name_length (f (line, 2));
			variant_name := to_variant_name (f (line, 2));
			log (text => "variant " & to_string (variant_name), level => log_threshold);
			
			
		elsif kw = keyword_package_model then -- package_model libraries/packages/S_SO14.pac
			expect_field_count (line, 2);

			-- The given path is something like libraries/packages/S_SO14.pac.
			-- Check if the package name like S_SO14 is too long or contains invalid characters.
			check_package_name_length (base_name (f (line, 2)));
			check_package_name_characters (to_package_name (base_name (f (line, 2))));

			package_model_name := to_package_model_name (f (line, 2));
			log (text => "package model " & to_string (package_model_name), level => log_threshold);
							
			-- Read package model 
			-- (like libraries/packages/__#__#lbr#bel_ic_pretty#S_SO14.pac)
			-- and do a conductor layer check if required.
			read_package (
				file_name		=> package_model_name, 
				check_layers	=> check_layers, 
				log_threshold	=> log_threshold);

			-- Create the link to the package model in the
			-- package library:
			variant.model_cursor := get_package_model (package_model_name);
		else
			invalid_keyword (kw);
		end if;
	end read_package_variant;




	
		
		
	procedure insert_package_variant is
		use pac_package_variants;
		
		inserted : boolean;
		position : pac_package_variants.cursor;
	begin
		check_variant_name_characters (variant_name); 
		-- CS move to procedure read_package_variant

		insert (
			container	=> variants,
			key			=> variant_name, -- N, D 
			inserted	=> inserted,
			position	=> position,
			new_item	=> variant);

		-- A particular variant must occur only once in the device model:
		if not inserted then
			log (ERROR, "variant " & to_string (variant_name) 
				& " already used !");
				
			raise constraint_error;
		end if;

		-- clean up for next variant
		variant := (others => <>);
	end insert_package_variant;


	
end et_device_read_package_variant;
