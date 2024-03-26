Public Class App_3D

    Public Sub CreateTestObject()

        Dim lines As New Dictionary(Of Integer, Tuple(Of Integer, Integer))
        Dim lineId As Integer = 1 ' Unique identifier for each line

        ' ============================


        Dim points3d As Point3D() = {
    New Point3D(1, -509, 30, -318),
    New Point3D(2, -509, 300, -318),
    New Point3D(3, -775, 30, -503),
    New Point3D(4, -775, 300, -503),
    New Point3D(5, -611, 30, -724),
    New Point3D(6, -611, 300, -724),
    New Point3D(7, -372, 30, -523),
    New Point3D(8, -372, 300, -523)
}


        Dim distanceThreshold As Double = 1000 ' Adjust based on your criteria for neighbor proximity
        Dim connections As New Dictionary(Of Integer, List(Of Integer))

        ' Initialize connections
        For Each point As Point3D In points3d
            connections(point.Id) = New List(Of Integer)
            For Each other As Point3D In points3d
                If point.Id <> other.Id AndAlso point.DistanceTo(other) <= distanceThreshold Then
                    connections(point.Id).Add(other.Id)
                End If
            Next
        Next

        ' Recursively refine connections
        RefineConnections(connections, points3d)

        ' ===================================

        ' Print refined connections
        For Each kvp As KeyValuePair(Of Integer, List(Of Integer)) In connections
            Console.WriteLine($"Point {kvp.Key} has connections to: {String.Join(", ", kvp.Value)}")

            Dim pointId As Integer = kvp.Key
            For Each connectedPointId In kvp.Value
                ' Add each connection as a line to the lines dictionary
                ' Ensure that each line is uniquely identified
                lines.Add(lineId, Tuple.Create(pointId, connectedPointId))
                lineId += 1 ' Increment the unique identifier for the next line
            Next

        Next

        ' ==================================================

        Console.WriteLine("Enter the spacing for the jagged line:")
        Dim spacing As Integer = Convert.ToInt32(Console.ReadLine())
        Console.WriteLine("Enter viewer's X, Y, Z coordinates separated by space (e.g., 10 20 30):")
        Dim viewerInput As String = Console.ReadLine()
        Dim viewerCoords As String() = viewerInput.Split(" "c)
        Dim viewer As New Vector3D_struct(Convert.ToDouble(viewerCoords(0)), Convert.ToDouble(viewerCoords(1)), Convert.ToDouble(viewerCoords(2)))

        Dim points As New Dictionary(Of Integer, Vector3D_struct)

        For Each point As Point3D In points3d
            points.Add(point.Id, New Vector3D_struct(point.X, point.Y, point.Z))
        Next

        Console.WriteLine("Enter the line number to draw:")
        Dim lineNumber As Integer = Convert.ToInt32(Console.ReadLine())

        If lines.ContainsKey(lineNumber) Then
            Dim line = lines(lineNumber)
            Dim origin = points(line.Item1)
            Dim destination = points(line.Item2)

            ' Determine drawing direction based on viewer's position using cross product
            Dim directionVector = Vector3D_struct.CrossProduct(destination - origin, viewer - origin)
            If directionVector.Z < 0 Then
                ' Swap origin and destination to ensure drawing from left to right
                Dim temp = origin
                origin = destination
                destination = temp
            End If

            Dim interpolatedPoints = InterpolateLine(origin, destination, spacing)
            ' Console.WriteLine($"Interpolated points for line {lineNumber}:")
            ' ====================================================================================

            For Each point In interpolatedPoints
                Dim newTuple2 = point.ToIntTuple

                Dim newTuple As (Integer, Integer, Integer) = (newTuple2.Item1, newTuple2.Item2, newTuple2.Item3)

                ' Console.WriteLine($"({point.X}, {point.Y}, {point.Z})")
                myTupleList.Add(newTuple)
            Next

            ' ====================================================================================
        Else
            Console.WriteLine("Invalid line number.")
        End If

        Console.ReadLine()
    End Sub



    Function InterpolateLine(origin As Vector3D_struct, destination As Vector3D_struct, spacing As Integer) As List(Of Vector3D_struct)
        Dim points As New List(Of Vector3D_struct)
        ' Calculate differences
        Dim dx = destination.X - origin.X
        Dim dy = destination.Y - origin.Y
        Dim dz = destination.Z - origin.Z

        ' Calculate steps required
        Dim steps = Math.Max(Math.Abs(dx), Math.Max(Math.Abs(dy), Math.Abs(dz))) \ spacing

        ' Calculate increment
        Dim xInc = dx \ steps
        Dim yInc = dy \ steps
        Dim zInc = dz \ steps

        ' Generate points
        For stepNumber As Integer = 0 To steps
            Dim x = origin.X + (stepNumber * xInc)
            Dim y = origin.Y + (stepNumber * yInc)
            Dim z = origin.Z + (stepNumber * zInc)
            points.Add(New Vector3D_struct(x, y, z))
        Next

        Return points
    End Function










    Sub RefineConnections(ByRef connections As Dictionary(Of Integer, List(Of Integer)), ByVal points As Point3D())
        Dim initialCount As Integer = CountLines(connections)
        Console.WriteLine("Initial line count: {0}", initialCount)

        Dim changesMade As Boolean = True

        While changesMade
            changesMade = False

            ' Create a copy of the connections to iterate over while modifying the original
            Dim currentConnections As New Dictionary(Of Integer, List(Of Integer))
            For Each kvp In connections
                currentConnections(kvp.Key) = New List(Of Integer)(kvp.Value)
            Next

            For Each pointId In currentConnections.Keys
                Dim neighbors As New List(Of Integer)(currentConnections(pointId))

                For Each neighborId In neighbors
                    ' Perform union/intersection on the connection pairs
                    Dim s1 = connections(pointId).Count
                    Dim k1 = connections(pointId).Contains(neighborId)
                    Dim s2 = connections(neighborId).Count
                    Dim k2 = connections(neighborId).Contains(pointId)

                    Dim unionResult = UnionOp(s1, k1, s2, k2)
                    Dim intersectResult = IntersectOp(s1, k1, s2, k2)

                    If Not intersectResult.k Then
                        ' Remove connection if intersection fails
                        connections(pointId).Remove(neighborId)
                        connections(neighborId).Remove(pointId)
                        changesMade = True
                        Console.WriteLine("Removed line between points {0} and {1}", pointId, neighborId)
                    ElseIf unionResult.s > connections(pointId).Count Then
                        ' Expand connection if union succeeds with greater size
                        For Each id In connections(neighborId)
                            If Not connections(pointId).Contains(id) Then
                                connections(pointId).Add(id)
                            End If
                        Next
                        changesMade = True
                        Console.WriteLine("Expanded line between points {0} and {1}", pointId, neighborId)
                    End If
                Next
            Next
        End While

        Dim finalCount As Integer = CountLines(connections)
        Console.WriteLine("Final line count: {0}", finalCount)
        Console.WriteLine("Lines removed: {0}", initialCount - finalCount)
    End Sub

    Private Function UnionOp(s1 As Integer, k1 As Boolean, s2 As Integer, k2 As Boolean) As (s As Integer, k As Boolean)
        If k1 And k2 Then
            Return (Math.Max(s1, s2), True)
        ElseIf k1 Then
            Return (s1, True)
        ElseIf k2 Then
            Return (s2, True)
        Else
            Return (Math.Min(s1, s2), False)
        End If
    End Function

    Private Function IntersectOp(s1 As Integer, k1 As Boolean, s2 As Integer, k2 As Boolean) As (s As Integer, k As Boolean)
        If k1 And k2 Then
            Return (Math.Min(s1, s2), True)
        ElseIf k1 Then
            Return (s2, False)
        ElseIf k2 Then
            Return (s1, False)
        Else
            Return (Math.Max(s1, s2), False)
        End If
    End Function

    Private Function CountLines(connections As Dictionary(Of Integer, List(Of Integer))) As Integer
        Dim lineCount As Integer = 0
        Dim visitedPairs As New HashSet(Of (Integer, Integer))

        For Each pointId In connections.Keys
            For Each neighborId In connections(pointId)
                Dim pair = If(pointId < neighborId, (pointId, neighborId), (neighborId, pointId))
                If Not visitedPairs.Contains(pair) Then
                    lineCount += 1
                    visitedPairs.Add(pair)
                End If
            Next
        Next

        Return lineCount
    End Function















    'Sub RefineConnections(ByRef connections As Dictionary(Of Integer, List(Of Integer)), ByVal points As Point3D())
    '    Dim changesMade As Boolean = True

    '    While changesMade
    '        changesMade = False

    '        ' Create a copy of the connections to iterate over while modifying the original
    '        Dim currentConnections As New Dictionary(Of Integer, List(Of Integer))
    '        For Each kvp In connections
    '            currentConnections(kvp.Key) = New List(Of Integer)(kvp.Value)
    '        Next

    '        For Each pointId In currentConnections.Keys
    '            Dim neighbors As New List(Of Integer)(currentConnections(pointId))

    '            For Each neighborId In neighbors
    '                ' Perform union/intersection on the connection pairs
    '                Dim s1 = connections(pointId).Count
    '                Dim k1 = connections(pointId).Contains(neighborId)
    '                Dim s2 = connections(neighborId).Count
    '                Dim k2 = connections(neighborId).Contains(pointId)

    '                Dim unionResult = UnionOp(s1, k1, s2, k2)
    '                Dim intersectResult = IntersectOp(s1, k1, s2, k2)

    '                If Not intersectResult.k Then
    '                    ' Remove connection if intersection fails
    '                    connections(pointId).Remove(neighborId)
    '                    connections(neighborId).Remove(pointId)
    '                    changesMade = True
    '                ElseIf unionResult.s > connections(pointId).Count Then
    '                    ' Expand connection if union succeeds with greater size
    '                    connections(pointId).Add(neighborId)
    '                    connections(neighborId).Add(pointId)
    '                    changesMade = True
    '                End If
    '            Next
    '        Next
    '    End While
    'End Sub

    'Private Function UnionOp(s1 As Integer, k1 As Boolean, s2 As Integer, k2 As Boolean) As (s As Integer, k As Boolean)
    '    If k1 And k2 Then
    '        Return (Math.Max(s1, s2), True)
    '    ElseIf k1 Then
    '        Return (s1, True)
    '    ElseIf k2 Then
    '        Return (s2, True)
    '    Else
    '        Return (Math.Min(s1, s2), False)
    '    End If
    'End Function

    'Private Function IntersectOp(s1 As Integer, k1 As Boolean, s2 As Integer, k2 As Boolean) As (s As Integer, k As Boolean)
    '    If k1 And k2 Then
    '        Return (Math.Min(s1, s2), True)
    '    ElseIf k1 Then
    '        Return (s2, False)
    '    ElseIf k2 Then
    '        Return (s1, False)
    '    Else
    '        Return (Math.Max(s1, s2), False)
    '    End If
    'End Function





    'Sub RefineConnections(ByRef connections As Dictionary(Of Integer, List(Of Integer)), ByVal points As Point3D())
    '    ' Determine the size of the binary grid based on the range of points
    '    Dim minX = Double.MaxValue, maxX = Double.MinValue
    '    Dim minY = Double.MaxValue, maxY = Double.MinValue
    '    Dim minZ = Double.MaxValue, maxZ = Double.MinValue

    '    For Each point As Point3D In points
    '        minX = Math.Min(minX, point.X)
    '        maxX = Math.Max(maxX, point.X)
    '        minY = Math.Min(minY, point.Y)
    '        maxY = Math.Max(maxY, point.Y)
    '        minZ = Math.Min(minZ, point.Z)
    '        maxZ = Math.Max(maxZ, point.Z)
    '    Next

    '    Dim gridSizeX = CInt(maxX - minX) + 1
    '    Dim gridSizeY = CInt(maxY - minY) + 1
    '    Dim gridSizeZ = CInt(maxZ - minZ) + 1

    '    ' Create the binary grid
    '    Dim grid(gridSizeX - 1, gridSizeY - 1, gridSizeZ - 1) As Boolean

    '    ' Map points to the binary grid
    '    For Each point As Point3D In points
    '        Dim gridX = CInt(point.X - minX)
    '        Dim gridY = CInt(point.Y - minY)
    '        Dim gridZ = CInt(point.Z - minZ)
    '        grid(gridX, gridY, gridZ) = True
    '    Next

    '    ' Iterate through each point and refine its neighbors
    '    For Each kvp As KeyValuePair(Of Integer, List(Of Integer)) In connections
    '        Dim pointId = kvp.Key
    '        Dim neighbors = kvp.Value
    '        Dim point = points(pointId - 1)

    '        ' Get the grid coordinates of the current point
    '        Dim gridX = CInt(point.X - minX)
    '        Dim gridY = CInt(point.Y - minY)
    '        Dim gridZ = CInt(point.Z - minZ)

    '        ' Check the 26-connected neighborhood in the grid
    '        For dx = -1 To 1
    '            For dy = -1 To 1
    '                For dz = -1 To 1
    '                    If dx = 0 AndAlso dy = 0 AndAlso dz = 0 Then Continue For

    '                    Dim newX = gridX + dx
    '                    Dim newY = gridY + dy
    '                    Dim newZ = gridZ + dz

    '                    If newX >= 0 AndAlso newX < gridSizeX AndAlso
    '                   newY >= 0 AndAlso newY < gridSizeY AndAlso
    '                   newZ >= 0 AndAlso newZ < gridSizeZ AndAlso
    '                   grid(newX, newY, newZ) Then

    '                        ' Find the neighbor point ID
    '                        For Each otherPoint As Point3D In points
    '                            If otherPoint.X = newX + minX AndAlso
    '                           otherPoint.Y = newY + minY AndAlso
    '                           otherPoint.Z = newZ + minZ Then

    '                                Dim neighborId = otherPoint.Id
    '                                If Not neighbors.Contains(neighborId) Then
    '                                    neighbors.Add(neighborId)
    '                                End If
    '                                Exit For
    '                            End If
    '                        Next
    '                    End If
    '                Next
    '            Next
    '        Next
    '    Next

    '    ' Remove duplicate lines
    '    Dim uniqueLines As New HashSet(Of Tuple(Of Integer, Integer))
    '    For Each kvp As KeyValuePair(Of Integer, List(Of Integer)) In connections
    '        Dim pointId = kvp.Key
    '        Dim neighbors = kvp.Value

    '        For Each neighborId In neighbors
    '            Dim line = Tuple.Create(Math.Min(pointId, neighborId), Math.Max(pointId, neighborId))
    '            uniqueLines.Add(line)
    '        Next
    '    Next

    '    ' Update the connections dictionary with the refined neighbors
    '    connections.Clear()
    '    For Each line In uniqueLines
    '        Dim pointId = line.Item1
    '        Dim neighborId = line.Item2

    '        If Not connections.ContainsKey(pointId) Then
    '            connections(pointId) = New List(Of Integer)
    '        End If
    '        connections(pointId).Add(neighborId)

    '        If Not connections.ContainsKey(neighborId) Then
    '            connections(neighborId) = New List(Of Integer)
    '        End If
    '        connections(neighborId).Add(pointId)
    '    Next
    'End Sub


    'Sub RefineConnections(ByRef connections As Dictionary(Of Integer, List(Of Integer)), ByVal points As Point3D())
    '    Dim changesMade As Boolean = True

    '    While changesMade
    '        changesMade = False

    '        ' Create a copy of the connections to iterate over while modifying the original
    '        Dim currentConnections = New Dictionary(Of Integer, List(Of Integer))(connections)

    '        For Each pointId In currentConnections.Keys
    '            Dim neighbors = New List(Of Integer)(currentConnections(pointId))

    '            For Each neighborId In neighbors
    '                ' Check mutual connections between the point and its neighbor
    '                Dim mutualConnections = connections(pointId).Intersect(connections(neighborId)).ToList()

    '                ' Further refine by ensuring there's a chain of mutual connections
    '                If Not HasChainOfMutualConnections(pointId, neighborId, connections, points) Then
    '                    connections(pointId).Remove(neighborId)
    '                    connections(neighborId).Remove(pointId) ' Ensure symmetry in connection removal
    '                    changesMade = True
    '                End If
    '            Next
    '        Next
    '    End While
    'End Sub












    Function HasChainOfMutualConnections(pointId As Integer, targetId As Integer, connections As Dictionary(Of Integer, List(Of Integer)), points As Point3D(), Optional visited As HashSet(Of Integer) = Nothing) As Boolean
        If visited Is Nothing Then visited = New HashSet(Of Integer)

        ' Avoid revisiting points
        If visited.Contains(pointId) Then Return False
        visited.Add(pointId)

        Dim neighbors = connections(pointId)
        If neighbors.Contains(targetId) Then
            ' Direct connection found
            Return True
        Else
            ' Recursively search for a chain of mutual connections
            For Each neighborId In neighbors
                If HasChainOfMutualConnections(neighborId, targetId, connections, points, visited) Then
                    Return True
                End If
            Next
        End If

        Return False
    End Function






    Structure Point3D
        Public Id As Integer
        Public X As Double
        Public Y As Double
        Public Z As Double

        Public Sub New(id As Integer, x As Double, y As Double, z As Double)
            Me.Id = id
            Me.X = x
            Me.Y = y
            Me.Z = z
        End Sub

        Public Function DistanceTo(other As Point3D) As Double
            Return Math.Sqrt((X - other.X) ^ 2 + (Y - other.Y) ^ 2 + (Z - other.Z) ^ 2)
        End Function
    End Structure

    Public Structure Vector3D_struct
        Public X As Double
        Public Y As Double
        Public Z As Double

        Public Sub New(x As Double, y As Double, z As Double)
            Me.X = x
            Me.Y = y
            Me.Z = z
        End Sub

        Public Shared Operator -(v1 As Vector3D_struct, v2 As Vector3D_struct) As Vector3D_struct
            Return New Vector3D_struct(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)
        End Operator

        Public Shared Function CrossProduct(v1 As Vector3D_struct, v2 As Vector3D_struct) As Vector3D_struct
            Dim x = v1.Y * v2.Z - v1.Z * v2.Y
            Dim y = v1.Z * v2.X - v1.X * v2.Z
            Dim z = v1.X * v2.Y - v1.Y * v2.X
            Return New Vector3D_struct(x, y, z)
        End Function
        Public Function ToIntTuple() As (Integer, Integer, Integer)
            Return (CInt(Fix(X)), CInt(Fix(Y)), CInt(Fix(Z)))
        End Function

    End Structure


End Class






































































































































































































