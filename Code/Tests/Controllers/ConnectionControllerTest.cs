using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.Connections;
using DDDNetCore.Domain.Connections.DTOS;
using DDDNetCore.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;
using System;

namespace DDDNetCore.Tests.Controllers
{
    public class ConnectionControllerTest
    {
        [Fact]
        public async Task GetReachablePlayers_ReturnsPlayersList()
        {
            // Arrange
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            List<PlayerDto> lst = new List<PlayerDto>{obj};
            string playerEmail = "test2@email.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(serv => serv.GetReachablePlayers(playerEmail))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetReachablePlayers(playerEmail);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, returnValue);
        }

        [Fact]
        public async Task GetMutualFriends_ReturnsPlayersList()
        {
            // Arrange
            GetMutualFriendsDto dto = new GetMutualFriendsDto("test@email.com");
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            List<PlayerDto> lst = new List<PlayerDto>{obj};
            string playerEmail = "test2@email.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(serv => serv.GetMutualFriends(playerEmail, dto))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetMutualFriends(playerEmail, dto);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, returnValue);
        }

        [Fact]
        public async Task GetNetwork_ReturnsPlayersList()
        {
            // Arrange
            GetNetworkDto dto = new GetNetworkDto(2);
            ConnectionDto obj = new ConnectionDto("1", "p1", "p2", 1, new List<string>{"tag1"});
            List<ConnectionDto> lst = new List<ConnectionDto>{obj};
            string playerEmail = "test2@email.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(serv => serv.GetNetwork(playerEmail, dto))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetNetwork(playerEmail, dto);

            // Assert
            var returnValue = Assert.IsType<List<ConnectionDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, returnValue);
        }

        //
        [Fact]
        public async Task GetAllConnections_ReturnsGettingConnectionDtoList()
        {
            // Arrange
            GettingConnectionDto obj = new("john", "test@gmail.com", 5,
                new List<string> { "tag"});
            List<GettingConnectionDto> lst = new() { obj };
            string playerEmail = "test@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.GetAllConnectionsAsync(playerEmail))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetAllConnections(playerEmail);

            // Assert
            var returnValue = Assert.IsType<List<GettingConnectionDto>>(result.Value);
            mockServ.Verify();
            Assert.Equal(lst, returnValue);
        }


        [Fact]
        public async Task GetAllConnections_WhenEmailNotValid()
        {
            // Arrange
            GettingConnectionDto obj = new("john", "test@gmail.com", 5,
                new List<string> { "tag" });
            List<GettingConnectionDto> lst = new() { obj };
            string playerEmail = "test@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.GetAllConnectionsAsync(playerEmail))
                .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetAllConnections(playerEmail);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }




        [Fact]
        public async Task GetByEmails_ReturnsConnectionDto()
        {
            // Arrange
            ConnectionDto obj = new(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(),
                3, new List<string> { "tag" });
            string playerEmail = "test@gmail.com";
            string friendEmail = "test2@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.GetByEmailsAsync(playerEmail, friendEmail))
                .ReturnsAsync(obj).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetByEmails(playerEmail, friendEmail);

            // Assert
            var returnValue = Assert.IsType<ConnectionDto>(result.Value);
            mockServ.Verify();
            Assert.Equal(obj, returnValue);
        }

        [Fact]
        public async Task GetByEmails_WhenEmailNotValid()
        {
            // Arrange
            ConnectionDto obj = new(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(),
                 3, new List<string> { "tag" });
            string playerEmail = "test@gmail.com";
            string friendEmail = "test2@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.GetByEmailsAsync(playerEmail, friendEmail))
                .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetByEmails(playerEmail, friendEmail);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }




        [Fact]
        public async Task UpdateTagsAndStrength_ReturnsConnectionDto()
        {
            // Arrange
            UpdatingConnectionDto param = new("test@gmail.com", "test2@gmail.com", 3, new List<string> { "tag"});
            ConnectionDto obj = new(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(),
                3, new List<string> { "tag" });
            string playerEmail = "test@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.UpdateTagsAndStrengthAsync(param))
                .ReturnsAsync(obj).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.UpdateTagsAndStrength(playerEmail, param);

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            
            
            var returnValue = Assert.IsType<ConnectionDto>(actionResult.Value);
            mockServ.Verify();
            
            Assert.Equal(obj, returnValue);
        }



        [Fact]
        public async Task UpdateTagsAndStrength_EmailNotEqual()
        {
            // Arrange
            UpdatingConnectionDto param = new("error", "error", 3, new List<string> { "tag" });
            ConnectionDto obj = new(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(),
                3, new List<string> { "tag" });
            string playerEmail = "test@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.UpdateTagsAndStrengthAsync(param))
                .ReturnsAsync(obj).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.UpdateTagsAndStrength(playerEmail, param);

            // Assert
            var actionResult = Assert.IsType<BadRequestResult>(result.Result);
            Assert.IsType<BadRequestResult>(actionResult);
        }

        [Fact]
        public async Task UpdateTagsAndStrength_WhenEmailNotValid()
        {
            // Arrange
            UpdatingConnectionDto param = new("test@gmail.com", "test2@gmail.com", 3, new List<string> { "tag" });
            ConnectionDto obj = new(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(),
                3, new List<string> { "tag" });
            string playerEmail = "test@gmail.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(s => s.UpdateTagsAndStrengthAsync(param))
                .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.UpdateTagsAndStrength(playerEmail, param);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }


    }
}