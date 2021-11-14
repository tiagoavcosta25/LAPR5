using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Controllers
{
    public class ConnectionRequestControllerTest
    {
        [Fact]
        public async Task ApproveRequest_ReturnsRequest()
        {
            // Arrange
            ApproveRequestDto dto = new ApproveRequestDto("request_pending", "middle_target_message");
            ConnectionRequestId cId = new ConnectionRequestId("12312322-4444-5555-6666-777888999000");

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.ApproveRequest(cId, dto))
                .ReturnsAsync(dto).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.ApproveRequest("12312322-4444-5555-6666-777888999000", dto);

            // Assert
            var actionValue = Assert.IsType<OkObjectResult>(result.Result);
            var returnValue = Assert.IsType<ApproveRequestDto>(actionValue.Value);
            mockServ.Verify();
            
            Assert.Equal(dto, returnValue);
        }

        [Fact]
        public async Task GetMiddleManRequests_ReturnsMiddleManRequests()
        {
            // Arrange
            IntroductionRequestDto dto = new IntroductionRequestDto("12312322-4444-5555-6666-777888999000", 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "request_pending", 1, new List<string>{"tag1"});
            ConnectionRequestId cId = new ConnectionRequestId("12312322-4444-5555-6666-777888999000");
            string playerEmail = "test@email.com";
            List<IntroductionRequestDto> lst = new List<IntroductionRequestDto>{dto};

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.GetMiddleManRequests(playerEmail))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.GetMiddleManRequests(playerEmail);

            // Assert
            var resultValue = Assert.IsType<List<IntroductionRequestDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, resultValue);
        }
    }
}