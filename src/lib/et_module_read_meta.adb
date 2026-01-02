------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / META DATA                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
-- - rename global variables
--
--

with ada.text_io;					use ada.text_io;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_section_headers;			use et_section_headers;
with et_keywords;					use et_keywords;

with et_meta;						use et_meta;
with et_time;						use et_time;

with et_general_rw;						use et_general_rw;



package body et_module_read_meta is

	use pac_generic_modules;

	

	meta_basic		: et_meta.type_basic;
	
	meta_schematic	: et_meta.type_schematic;
	prf_libs_sch	: et_meta.pac_preferred_libraries_schematic.list;

	meta_board		: et_meta.type_board;
	prf_libs_brd	: et_meta.pac_preferred_libraries_board.list;




	procedure add_meta_schematic is begin
		-- Add the so far collected basic meta data AND the 
		-- preferred schematic libs to schematic related meta data:
		meta_schematic := (meta_basic with 
			preferred_libs => prf_libs_sch);

		-- This clean up is not really required since
		-- section meta and preferred libs for schematic
		-- exist only once in the module file:
		prf_libs_sch.clear;
		
		-- Clean up basic meta stuff because
		-- it will be used for the board also:
		meta_basic := (others => <>);
	end add_meta_schematic;



	

	procedure add_meta_board is begin
		-- Add the so far collected basic meta data AND the 
		-- preferred board libs to board related meta data:
		meta_board := (meta_basic with
			preferred_libs => prf_libs_brd);

		-- This clean up is not really required since
		-- section meta and preferred libs for board
		-- exist only once in the module file:
		prf_libs_brd.clear;
		
		-- Clean up basic meta stuff because
		-- it will be used for the schematic also:
		meta_basic := (others => <>);
	end add_meta_board;



	
	
	procedure set_meta (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)				   
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_meta.pac_preferred_libraries_schematic;
		begin
			-- CS check whether date drawn <= date checked <= date_approved
			--  use type_basic for the test of schematic and board data.
			
			module.meta.schematic := meta_schematic;
			module.meta.board := meta_board;
		end;


	begin
		log (text => "meta data ...", level => log_threshold + 1);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
	end set_meta;



	

	
	-- Reads basic meta data. If given line does not contain
	-- basic meta stuff, returns a false.
	function read_meta_basic (
		line : in type_fields_of_line)
		return boolean 
	is
		use et_time;
		kw : constant string := f (line, 1);
		result : boolean := true;
	begin
		if kw = keyword_company then
			expect_field_count (line, 2);
			meta_basic.company := to_company (f (line, 2));

		elsif kw = keyword_customer then
			expect_field_count (line, 2);
			meta_basic.customer := to_customer (f (line, 2));
			
		elsif kw = keyword_partcode then
			expect_field_count (line, 2);
			meta_basic.partcode := to_partcode (f (line, 2));
			
		elsif kw = keyword_drawing_number then
			expect_field_count (line, 2);
			meta_basic.drawing_number := to_drawing_number (f (line, 2));
			
		elsif kw = keyword_revision then
			expect_field_count (line, 2);
			meta_basic.revision := to_revision (f (line, 2));
			
		elsif kw = keyword_drawn_by then
			expect_field_count (line, 2);
			meta_basic.drawn_by := to_person (f (line, 2));
			
		elsif kw = keyword_drawn_date then
			expect_field_count (line, 2);
			meta_basic.drawn_date := to_date (f (line, 2));
			
		elsif kw = keyword_checked_by then
			expect_field_count (line, 2);
			meta_basic.checked_by := to_person (f (line, 2));
			
		elsif kw = keyword_checked_date then
			expect_field_count (line, 2);
			meta_basic.checked_date := to_date (f (line, 2));
			
		elsif kw = keyword_approved_by then
			expect_field_count (line, 2);
			meta_basic.approved_by := to_person (f (line, 2));
			
		elsif kw = keyword_approved_date then
			expect_field_count (line, 2);
			meta_basic.approved_date := to_date (f (line, 2));

		else
			result := false;
		end if;
		
		return result;
	end read_meta_basic;



	
	
	procedure read_meta_schematic (
		line : in type_fields_of_line)
	is 
		kw : constant string := f (line, 1);
	begin
		-- first parse line for basic meta stuff.
		-- if no meta stuff found, test for schematic specific meta data:
		if read_meta_basic (line) = false then
			-- CS: in the future, if there is schematic specific meta data:
			-- if kw = keyword_xyz then
			-- do something
			--else
			invalid_keyword (kw);
		end if;
	end;




	
	procedure read_meta_board (
		line : in type_fields_of_line)
	is 
		kw : constant string := f (line, 1);
	begin
		-- first parse line for basic meta stuff.
		-- if no meta stuff found, test for bord specific meta data:
		if read_meta_basic (line) = false then
			-- CS: in the future, if there is schematic specific meta data:
			-- if kw = keyword_xyz then
			-- do something
			--else
			invalid_keyword (kw);
		end if;
	end;		



	



	procedure read_preferred_lib_schematic (
 		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
		use et_meta;
		lib : pac_preferred_library_schematic.bounded_string;
	begin
		if kw = keyword_path then
			expect_field_count (line, 2);

			lib := to_preferred_library_schematic (f (line, 2));
			
			if not exists (lib) then
				log (WARNING, "Preferred library path for devices " 
					& enclose_in_quotes (to_string (lib))
					& " does not exist !");
			end if;

			-- Collect the library path in temporarily list:
			prf_libs_sch.append (lib);
		else
			invalid_keyword (kw);
		end if;
	end read_preferred_lib_schematic;




	
	
	procedure read_preferred_lib_board (
 		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
		use et_meta;
		lib : pac_preferred_library_board.bounded_string;
	begin
		if kw = keyword_path then
			expect_field_count (line, 2);
			lib := to_preferred_library_board (f (line, 2));

			if not exists (lib) then
				log (WARNING, "Preferred library path for non-electrical packages " 
					& enclose_in_quotes (to_string (lib))
					& " does not exist !");
			end if;
			
			-- Collect the library path in temporarily list:
			prf_libs_brd.append (lib);
		else
			invalid_keyword (kw);
		end if;
	end read_preferred_lib_board;

	
	
end et_module_read_meta;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
