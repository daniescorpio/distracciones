with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- constants 
    -----------------------------------------------------------------------
    
    MOSTRAR_INFO_DISPLAY_PRIORITY  	  : Integer   := 1;
    LEER_POSICION_CABEZA_PRIORITY 	  : Integer   := 2;
    LEER_GIRO_VOLANTE_PRIORITY 	  	  : Integer   := 3;
    LEER_DISTANCIA_PRIORITY 	  	  : Integer   := 4;
    CALCULAR_RIESGOS_PRIORITY	  	  : Integer   := 5;
    
    INTERVALO_DETECCION_RIESGOS		  : Time_Span := Milliseconds (150);
    INTERVALO_LECTURA_DISTANCIA_SEGURIDAD : Time_Span := Milliseconds (300);
    INTERVALO_LECTURA_GIRO_VOLANTE        : Time_Span := Milliseconds (350);
    INTERVALO_LECTURA_POSICION_CABEZA     : Time_Span := Milliseconds (400);
    INTERVALO_REFRESCO_DISPLAY		  : Time_Span := Milliseconds (1000);
    
    
    -----------------------------------------------------------------------
    ------------- custom types 
    -----------------------------------------------------------------------
    
    type Cabeza_Inclinada_Estado_Type is (CABEZA_INCLINADA, CABEZA_NO_INCLINADA);
    type Volantazo_Estado_Type 	      is (ESTADO_VOLANTAZO, ESTADO_NO_VOLANTAZO);
    type Distancia_Estado_Type 	      is (DISTANCIA_PELIGROSA, DISTANCIA_INSEGURA, DISTANCIA_IMPRUDENTE, DISTANCIA_SEGURA);
    
    -----------------------------------------------------------------------
    ------------- declaration of protected objects 
    -----------------------------------------------------------------------

    Protected Sintomas is
      procedure Guardar_Estado_Distraccion (actual_Distraccion : in Cabeza_Inclinada_Estado_Type);
      procedure Guardar_Estado_Volantazo   (actual_Volantazo   : in Volantazo_Estado_Type);
      procedure Guardar_Estado_Distancia   (actual_Distancia   : in Distancia_Estado_Type);
      function  Obtener_Estado_Distraccion return Cabeza_Inclinada_Estado_Type;
      function  Obtener_Estado_Volantazo   return Volantazo_Estado_Type;
      function  Obtener_Estado_Distancia   return Distancia_Estado_Type;
    private
      CABEZA_INCLINADA : Cabeza_Inclinada_Estado_Type;
      VOLANTAZO	       : Volantazo_Estado_Type;
      DISTANCIA	       : Distancia_Estado_Type;
      
    end Sintomas;
    
    Protected Medidas is
      procedure Guardar_Distancia_Actual (actual_Distancia : in Distance_Samples_Type);
      procedure Guardar_Velocidad_Actual (actual_Velocidad : in Speed_Samples_Type);
      function  Obtener_Distancia_Actual return Distance_Samples_Type;
      function  Obtener_Velocidad_Actual return Speed_Samples_Type;
    private
     DISTANCIA : Distance_Samples_Type;
     VELOCIDAD : Speed_Samples_Type;
    end Medidas;
    
    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR
    
    task LeerPosicionCabeza is 
      pragma priority (LEER_POSICION_CABEZA_PRIORITY);
    end leerPosicionCabeza;
    
    task LeerDistancia is 
      pragma priority (LEER_DISTANCIA_PRIORITY);
    end LeerDistancia;
    
    task LeerGiroVolante is
      pragma priority (LEER_GIRO_VOLANTE_PRIORITY);
    end leerGiroVolante;
    
    task MostrarInfoDisplay is
      pragma priority (MOSTRAR_INFO_DISPLAY_PRIORITY);
    end; 
    
    task CalcularRiesgos is
      pragma priority (CALCULAR_RIESGOS_PRIORITY);
    end;


    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------
    
    -----------------------------------------------------------------------
    ------------- TAREA POSICION CABEZA 
    -----------------------------------------------------------------------
    
    task body LeerPosicionCabeza is 
      Previous_H 	 : HeadPosition_Samples_Type := (0, 0);
      Current_H  	 : HeadPosition_Samples_Type;
      Current_S     	 : Steering_Samples_Type;
      Distraccion 	 : Cabeza_Inclinada_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
    
      Siguiente_instante := Clock + INTERVALO_LECTURA_POSICION_CABEZA;
      
      loop
        
        Starting_Notice ("Tarea lectura de cabeceo iniciada.");
      
        Distraccion := CABEZA_NO_INCLINADA;
      
        Reading_HeadPosition(Current_H);
        Reading_Steering (Current_S);
        
        if (Current_H(x) > 30 AND Previous_H(x) > 30) then 
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        if (Current_H(x) < -30 AND Previous_H(x) < -30) then
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        if (Current_H(y) > 30 AND Previous_H(y) > 30 AND Current_S <= 0) then 
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        if (Current_H(y) < -30 AND Previous_H(y) < -30 AND Current_S >= 0) then
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        Previous_H := Current_H;
        Sintomas.Guardar_Estado_Distraccion(Distraccion);
        Distraccion := CABEZA_NO_INCLINADA;
        
        Finishing_Notice ("Tarea lectura de cabeceo finalizada.");
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_POSICION_CABEZA;
        
        
        
      end loop;
      
    end leerPosicionCabeza;
    
    
    
    -----------------------------------------------------------------------
    ------------- TAREA DISTANCIA DE SEGURIDAD 
    -----------------------------------------------------------------------

     task body LeerDistancia is
       Current_D 	   : Distance_Samples_Type := 0;
       Current_V 	   : Speed_Samples_Type    := 0;
       Distancia_Seguridad : Float		   := 0.0;
       Distancia_Sintoma   : Distancia_Estado_Type;
       Siguiente_Instante  : Time;
 
     begin
     
       Siguiente_instante := Clock + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
	
       loop
       
         Starting_Notice ("Tarea lectura distancia de seguridad iniciada.");
      
	 Distancia_Sintoma := DISTANCIA_SEGURA;

         Reading_Distance (Current_D);      
         Reading_Speed (Current_V);
	 Distancia_Seguridad := (Float(Current_V) / 10.0) ** 2;
        
         if (Float(Current_D) < Distancia_Seguridad) then
           Distancia_Sintoma := DISTANCIA_INSEGURA;
         elsif(Float(Current_D) < Distancia_Seguridad / 2.0) then
	   Distancia_Sintoma := DISTANCIA_IMPRUDENTE;
	 elsif(Float(Current_D) < Distancia_Seguridad / 3.0 ) then
	   Distancia_Sintoma := DISTANCIA_PELIGROSA;
	 end if;

	Medidas.Guardar_Distancia_Actual(Current_D);
	Medidas.Guardar_Velocidad_Actual(Current_V);
	Sintomas.Guardar_Estado_Distancia(Distancia_Sintoma);
	Distancia_Sintoma := DISTANCIA_SEGURA;	
	
        Finishing_Notice ("Tarea lectura distancia de seguridad finalizada.");

	delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
        
    	
    	
        end loop;
      
    end leerDistancia;
    
    
    
    -----------------------------------------------------------------------
    ------------- TAREA GIRO VOLANTE 
    -----------------------------------------------------------------------
    
    task body LeerGiroVolante is
      Current_V 	 : Speed_Samples_Type    := 0;
      Previous_S    	 : Steering_Samples_Type := 0;
      Current_S     	 : Steering_Samples_Type;
      Actual_Offset 	 : Steering_Samples_Type;
      Volantazo   	 : Volantazo_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_LECTURA_GIRO_VOLANTE;
      
      loop
      
        Starting_Notice ("Tarea lectura datos volante iniciada.");
        
        Volantazo := ESTADO_NO_VOLANTAZO;
        
        Reading_Steering (Current_S);
        Current_V := Medidas.Obtener_Velocidad_Actual;
        
        Actual_Offset := Current_S - Previous_S;
        
        if ((Actual_Offset > 20 OR Actual_Offset < -20) AND Current_V > 40) then 
          Volantazo := ESTADO_VOLANTAZO;
        end if;
        
        Previous_S := Current_S;
        Sintomas.Guardar_Estado_Volantazo(Volantazo);
        Volantazo := ESTADO_NO_VOLANTAZO;
        
        Finishing_Notice ("Tarea lectura datos volante finalizada.");
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_GIRO_VOLANTE;
              
        
        
      end loop;
      
    end leerGiroVolante;
    
    
 
    -----------------------------------------------------------------------
    ------------- TAREA MOSTRAR INFO DISPLAY 
    -----------------------------------------------------------------------
        
    task body MostrarInfoDisplay is
      Distraccion 	 : Cabeza_Inclinada_Estado_Type;
      Distancia_Sintoma	 : Distancia_Estado_Type;
      Volantazo   	 : Volantazo_Estado_Type;
      Velocidad		 : Speed_Samples_Type;
      Distancia 	 : Distance_Samples_Type;
      Siguiente_Instante : Time;

    begin
      
      Siguiente_instante := Clock + INTERVALO_REFRESCO_DISPLAY;
      
      delay until Siguiente_Instante;
      Siguiente_Instante := Siguiente_Instante + INTERVALO_REFRESCO_DISPLAY;
      
      loop
        
        Starting_Notice ("Tarea mostrar informacion en display iniciada.");
        
	Distancia := Medidas.Obtener_Distancia_Actual;
	Velocidad := Medidas.Obtener_Velocidad_Actual;
	
	New_Line;
	New_Line;
	put ("-----------------------------------------------------");
	Display_Distance (Distancia);
	Display_Speed (Velocidad);
	New_Line;


        Distraccion 	  := Sintomas.Obtener_Estado_Distraccion;
	Distancia_Sintoma := Sintomas.Obtener_Estado_Distancia;
	Volantazo   	  := Sintomas.Obtener_Estado_Volantazo;
	
	if (Distraccion	      /= CABEZA_NO_INCLINADA OR 
	    Distancia_Sintoma /= DISTANCIA_SEGURA    OR 
	    Volantazo         /= ESTADO_NO_VOLANTAZO) 
	then
	
	  if (Distraccion /= CABEZA_NO_INCLINADA) then
	    Put (" Se han detectado movimientos inusuales en su cabeza ");
	    New_line;
	  end if;

          if (Distancia_Sintoma /= DISTANCIA_SEGURA) then
	    Put (" La distancia respecto al coche de alante es inapropiada");
	    New_line;
	  end if;

	  if(Volantazo /= ESTADO_NO_VOLANTAZO) then
	    Put(" Se han detectado movimientos inusuales en el volante ");
	    New_line;
	  end if;
	
	end if;
	
	put ("-----------------------------------------------------");
	New_Line;
	New_Line;
	
        Finishing_Notice ("Tarea mostrar informacion en display finalizada.");
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_REFRESCO_DISPLAY;
        
        
        
      end loop;
      
    end;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA DETECCION DE RIESGOS 
    -----------------------------------------------------------------------
        
    task body CalcularRiesgos is
      Distraccion 	 : Cabeza_Inclinada_Estado_Type;
      Volantazo 	 : Volantazo_Estado_Type;
      Distancia_Sintoma  : Distancia_Estado_Type;
      Velocidad		 : Speed_Samples_Type;      
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_DETECCION_RIESGOS;
      
      
      delay until Siguiente_Instante;
      Siguiente_Instante := Siguiente_Instante + INTERVALO_DETECCION_RIESGOS;
      
      loop
	
	Starting_Notice ("Tarea deteccion de riesgos iniciada.");
	
        Volantazo := Sintomas.Obtener_Estado_Volantazo;
        if (Volantazo = ESTADO_VOLANTAZO) then
          Beep(1);
        end if;

	Distraccion := Sintomas.Obtener_Estado_Distraccion;
	Velocidad   := Medidas.Obtener_Velocidad_Actual;
        if (Distraccion = CABEZA_INCLINADA) then
          if (Velocidad > 70) then
            Beep(3);
          else 
            Beep(2);
          end if; 
        end if;
        
        Distancia_Sintoma := Sintomas.Obtener_Estado_Distancia;
	if (Distancia_Sintoma = DISTANCIA_INSEGURA) then
	  Light(On);
	elsif (Distancia_Sintoma = DISTANCIA_IMPRUDENTE) then
	  Light(On);
	  Beep(4);
        end if;

	if (Distancia_Sintoma = DISTANCIA_PELIGROSA AND Distraccion = CABEZA_INCLINADA) then
	  Beep(5);
	  Activate_Brake;
	end if;
	
	Finishing_Notice ("Tarea deteccion de riesgos finalizada.");

        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_DETECCION_RIESGOS;
        
      end loop;
      
    end;
    
    
    
    -----------------------------------------------------------------------
    ------------- body of protected objects 
    -----------------------------------------------------------------------
    
    protected body Sintomas is
      
      procedure Guardar_Estado_Distraccion (actual_Distraccion: in Cabeza_Inclinada_Estado_Type) is
      begin
        CABEZA_INCLINADA := actual_Distraccion;
      end Guardar_Estado_Distraccion;
      
      procedure Guardar_Estado_Volantazo (actual_Volantazo : in Volantazo_Estado_Type) is 
      begin
        VOLANTAZO := actual_Volantazo;
      end Guardar_Estado_Volantazo;
      
      procedure Guardar_Estado_Distancia (actual_Distancia : in Distancia_Estado_Type) is
      begin
      	DISTANCIA := actual_Distancia;
      end Guardar_Estado_Distancia;
      
      function Obtener_Estado_Distraccion return Cabeza_Inclinada_Estado_Type is
      begin 
        return CABEZA_INCLINADA;
      end Obtener_Estado_Distraccion;
      
      function Obtener_Estado_Volantazo return Volantazo_Estado_Type is
      begin
        return VOLANTAZO;
      end Obtener_Estado_Volantazo;
      
      function Obtener_Estado_Distancia return Distancia_Estado_Type is
      begin
        return DISTANCIA;
      end Obtener_Estado_Distancia;
    end Sintomas;
    
    
    protected body Medidas is
    
      procedure Guardar_Distancia_Actual (actual_Distancia : in Distance_Samples_Type) is 
      begin
        DISTANCIA := actual_Distancia;
      end Guardar_Distancia_Actual;

      procedure Guardar_Velocidad_Actual (actual_Velocidad : in Speed_Samples_Type) is 
      begin
        VELOCIDAD := actual_Velocidad;
      end Guardar_Velocidad_Actual;
      
      function Obtener_Distancia_Actual return Distance_Samples_Type is
      begin
        return DISTANCIA;
      end Obtener_Distancia_Actual;
      
      function Obtener_Velocidad_Actual return Speed_Samples_Type is
      begin
        return VELOCIDAD;
      end Obtener_Velocidad_Actual;
      
    end Medidas;
    
begin
   null;
end add;



